package have

import (
	"fmt"
	"strconv"
	"strings"
)
import "github.com/davecgh/go-spew/spew"

type Parser struct {
	lex         *Lexer
	tokensBuf   []*Token
	indentStack []string
	identStack  *IdentStack
	//branchStmts BranchStmtsMap
	//branchStmts *BranchStmtsTree
	branchTreesStack []*BranchStmtsTree

	ignoreUnknowns, dontLookup bool

	prevLbl *LabelStmt // Just declared labal is stored here temporarily
}

// BranchStmtsMap is used to store branch statements (break, continue, goto) that
// haven't been paired with their labels/statements yet. When parser
// finishes parsing blocks of code/branchable statments, it looks into
// this structure to see if there were any matches.
type BranchStmtsMap map[string][]*BranchStmt

// BranchStmtsTree stores a hierarchy of BranchStmtsMaps. Each nested block of code
// has its own BranchStmtsMap. This tree is needed to detect situations where gotos
// jump into blocks of code (only jumping outside/within blocks is possible).
type BranchStmtsTree struct {
	Members  BranchStmtsMap
	Children []*BranchStmtsTree
}

func NewBranchStmtsTree() *BranchStmtsTree {
	return &BranchStmtsTree{
		Members: BranchStmtsMap{},
	}
}

func (p *Parser) topBranchStmtsTree() *BranchStmtsTree {
	return p.branchTreesStack[len(p.branchTreesStack)-1]
}

func (p *Parser) pushNewBranchStmtsTree() *BranchStmtsTree {
	b := p.topBranchStmtsTree().NewChild()
	p.branchTreesStack = append(p.branchTreesStack, b)
	return b
}

func (p *Parser) popBranchStmtsTree() {
	p.branchTreesStack = p.branchTreesStack[:len(p.branchTreesStack)-1]
}

func (b *BranchStmtsTree) FindAll(label string) []*BranchStmt {
	result := []*BranchStmt{}
	result = append(result, b.Members.FindAll(label)...)
	for _, child := range b.Children {
		result = append(result, child.FindAll(label)...)
	}
	return result
}

func (b *BranchStmtsTree) CountBranchStmts() int {
	sum := len(b.Members)
	for _, child := range b.Children {
		sum += child.CountBranchStmts()
	}
	return sum
}

// Call MatchGotoLabels on every BranchStmtsMap in the tree.
func (b *BranchStmtsTree) MatchGotoLabels(labels map[string]*LabelStmt) {
	b.Members.MatchGotoLabels(labels)
	for _, child := range b.Children {
		child.MatchGotoLabels(labels)
	}
}

// Call MatchBranchableStmt on every BranchStmtsMap in the tree.
func (b *BranchStmtsTree) MatchBranchableStmt(branchable Stmt, lblName string, allowedBranchStmts ...TokenType) {
	b.Members.MatchBranchableStmt(branchable, lblName, allowedBranchStmts...)
	for _, child := range b.Children {
		child.MatchBranchableStmt(branchable, lblName, allowedBranchStmts...)
	}
}

func (b *BranchStmtsTree) NewChild() *BranchStmtsTree {
	r := NewBranchStmtsTree()
	b.Children = append(b.Children, r)
	return r
}

func (b BranchStmtsMap) Add(bs *BranchStmt) {
	lbl := ""
	if bs.Right != nil {
		lbl = bs.Right.name
	}
	b[lbl] = append(b[lbl], bs)
}

func (b BranchStmtsMap) FindAll(label string) []*BranchStmt {
	return b[label]
}

func (b BranchStmtsMap) Remove(bs *BranchStmt) {
	label := bs.Right.name
	for i, x := range b[label] {
		if x == bs {
			b[label] = append(b[label][:i], b[label][i+1:]...)
			if len(b[label]) == 0 {
				delete(b, label)
			}
			return
		}
	}
}

func (b BranchStmtsMap) MatchGotoLabels(labels map[string]*LabelStmt) {
	for labelName, label := range labels {
		matches := b[labelName]
		if len(matches) == 0 {
			continue
		}

		for i := len(matches) - 1; i >= 0; i-- {
			if matches[i].Token.Type == TOKEN_GOTO {
				matches[i].GotoLabel = label
				matches = append(matches[:i], matches[i+1:]...)
			}
		}

		if len(matches) == 0 {
			delete(b, labelName)
		}
	}
}

func (b BranchStmtsMap) MatchBranchableStmt(branchable Stmt, lblName string, allowedBranchStmts ...TokenType) {
	unnamed, ok := b[lblName]
	switch {
	case !ok:
		return
	case len(unnamed) == 0:
		delete(b, lblName)
	}
	for i := len(unnamed) - 1; i >= 0; i-- {
		x := unnamed[i]
		for _, y := range allowedBranchStmts {
			if y == x.Token.Type {
				x.Branchable = branchable
				unnamed = append(unnamed[:i], unnamed[i+1:]...)
			}
		}
	}
	if len(unnamed) > 0 {
		b[lblName] = unnamed
	} else {
		delete(b, lblName)
	}
}

// Stack of scopes available to the piece of code that is currently
// being parsed. It is a living stack, scopes are pushed to and popped
// from it as new blocks of code start and end.
// It is used for initial bonding of names and objects (packages,
// variables, types), which later helps the type checker.
type IdentStack []map[string]Object

func (is *IdentStack) pushScope() {
	*is = append(*is, map[string]Object{})
}

func (is *IdentStack) popScope() {
	*is = (*is)[:len(*is)-1]
}

func (is *IdentStack) empty() bool {
	return len(*is) == 0
}

func (is *IdentStack) addObject(v Object) {
	(*is)[len(*is)-1][v.Name()] = v
}

// Returns nil when not found
func (is *IdentStack) findObject(name string) Object {
	if decl, ok := GetBuiltinType(name); ok {
		return decl
	}
	for i := len(*is) - 1; i >= 0; i-- {
		if v, ok := (*is)[i][name]; ok {
			return v
		}
	}
	return nil
}

// Returns nil when not found
func (is *IdentStack) findTypeDecl(name string) *TypeDecl {
	if decl, ok := GetBuiltinType(name); ok {
		return decl
	}
	for i := len(*is) - 1; i >= 0; i-- {
		if v, ok := (*is)[i][name]; ok && v.ObjectType() == OBJECT_TYPE {
			return v.(*TypeDecl)
		}
	}
	return nil
}

func (p *Parser) nextToken() *Token {
	if len(p.tokensBuf) > 0 {
		result := p.tokensBuf[0]
		p.tokensBuf = p.tokensBuf[1:]
		return result
	}
	tok, err := p.lex.Next()
	if err != nil {
		panic(err) // TODO: not panic, return error
	}
	return tok
}

// See the next token without changing the parser state.
func (p *Parser) peek() *Token {
	t := p.nextToken()
	p.putBack(t)
	return t
}

func NewParser(lex *Lexer) *Parser {
	parser := NewParserWithoutBuiltins(lex)
	parser.loadBuiltinFuncs()
	return parser
}

func NewParserWithoutBuiltins(lex *Lexer) *Parser {
	return &Parser{lex: lex,
		identStack:       &IdentStack{map[string]Object{}},
		branchTreesStack: []*BranchStmtsTree{NewBranchStmtsTree()}}
}

// Put back a token.
func (p *Parser) putBack(tok *Token) {
	if tok == nil {
		panic(fmt.Errorf("NIL tok %s", tok.Type))
	}
	p.tokensBuf = append([]*Token{tok}, p.tokensBuf...)
}

// Put back a stack of tokens. It means that tokens are put back
// from the last to the first one.
func (p *Parser) putBackStack(tokenStack []*Token) {
	for i := len(tokenStack) - 1; i >= 0; i-- {
		p.putBack(tokenStack[i])
	}
}
func (p *Parser) expect(typ TokenType) *Token {
	token := p.nextToken()
	if token.Type != typ {
		// TODO: error msg here maybe?
		p.putBack(token)
		return nil
	}
	return token
}

// The stack is not changed when the result is false, and if it is true,
// then all expected tokens are consumed.
func (p *Parser) expectSeries(types ...TokenType) ([]*Token, bool) {
	stack := []*Token{}
	for _, typ := range types {
		t := p.nextToken()
		stack = append(stack, t)

		if t.Type != typ {
			p.putBackStack(stack)
			return nil, false
		}
	}
	return stack, true
}

func (p *Parser) skipWhiteSpace() {
	t := p.nextToken()
	// TOKEN_INDENT is the only whitespace token we currently have.
	// Once we have tokens for comments they probably should be
	// handled here as well.
	for ; t.Type == TOKEN_INDENT; t = p.nextToken() {
	}
	p.putBack(t)
}

// Use it in a place where you expect a new indented block of code
// to start.
// `err` not being nil indicates some indent mismatch.
func (p *Parser) expectNewIndent() (*Token, error) {
	//indent := p.expect(TOKEN_INDENT)
	indent := p.nextToken()
	if indent.Type != TOKEN_INDENT {
		return nil, fmt.Errorf("New indent expected, got %#v", indent)
	}

	prevIndent := ""
	if len(p.indentStack) > 0 {
		prevIndent = p.indentStack[len(p.indentStack)-1]
	}
	newIndent := indent.Value.(string)

	if !strings.HasPrefix(newIndent, prevIndent) || len(newIndent) == len(prevIndent) {
		return nil, fmt.Errorf("Code block is not indented")
	}

	p.indentStack = append(p.indentStack, newIndent)
	return indent, nil
}

// Tells if the current indent block ends here. Additionally,
// returns a parse error if it notices something wrong.
// It doesn't change the parser state (as opposed to handleIndentEnd).
func (p *Parser) isIndentEnd() (end bool, err error) {
	token := p.expect(TOKEN_INDENT)
	if token == nil {
		return false, fmt.Errorf("Indent expected, got %s", token.Type)
	}
	defer p.putBack(token)

	ident := token.Value.(string)
	curIdent := ""
	if len(p.indentStack) > 0 {
		curIdent = p.indentStack[len(p.indentStack)-1]
	}
	if curIdent != ident {
		if len(ident) >= len(curIdent) {
			return false, fmt.Errorf("Unexpected indent")
		}
		return true, nil
	}
	return false, nil
}

// Use it in a place where you expect another line of an indented
// block of code.
// If `end` is true then this indented block ends here, and parser
// will be pointed to the beginning of the next line.
// `err` not being nil indicates some indent mismatch.
func (p *Parser) handleIndentEnd() (end bool, err error) {
	end, err = p.isIndentEnd()
	if end {
		// Pop current indent
		p.indentStack = p.indentStack[:len(p.indentStack)-1]
		return
	}
	p.expect(TOKEN_INDENT)
	return
}

// This is very similar to handleIndentEnd, but the indented block
// of code can also be ended by an occurence of a token (not necessarily
// preceded by an end of indentation, or precended by an unmatched indent).
// Example:
// var y = struct:
//     x int
//      {x: 1}  // <- '{' ends the indented block of code
// Another:
// var y = struct:
//     x int
//   {x: 1}  // <- unmatched indent, but it's all right
// The special character is put back to the tokenizer, so that things
// like compount initializers of nested structures work.
func (p *Parser) handleIndentEndOrToken(tokenType TokenType) (end bool, err error) {
	end, err = p.handleIndentEnd()
	if end {
		return end, err
	}
	next := p.nextToken()
	defer p.putBack(next)
	if next.Type == tokenType {
		return true, nil
	}
	return false, err
}

// Very similar to handleIndentEndOrToken, but checks if the next token
// is NOT one of tokenTypes type.
func (p *Parser) handleIndentEndOrNoToken(tokenTypes ...TokenType) (end bool, err error) {
	end, err = p.handleIndentEnd()
	if end {
		return end, err
	}
	next := p.nextToken()
	defer p.putBack(next)

	for _, tokenType := range tokenTypes {
		if next.Type == tokenType {
			return false, err
		}
	}
	return true, nil
}

// Use this to check for the beginning of a statement branch (part of a statement
// on the same indent level as the statement itself). Examples: 'else' and 'elif'
// blocks.
func (p *Parser) checkForBranch(branchTokens ...TokenType) (ok bool, token *Token) {
	tokens := map[TokenType]bool{}
	for _, t := range branchTokens {
		tokens[t] = true
	}

	indTok := p.peek()
	if indTok.Type == TOKEN_EOF {
		return false, nil
	}

	end, err := p.isIndentEnd()
	if end || err != nil {
		return false, nil
	}

	// Consume it so that we can access next token
	p.expect(TOKEN_INDENT)

	if t := p.nextToken(); tokens[t.Type] {
		return true, t
	} else {
		p.putBack(t)
		p.putBack(indTok)
	}
	return false, nil
}

// Forces end of the current indent, can be used to end it
// in the middle of a line.
func (p *Parser) forceIndentEnd() {
	p.indentStack = p.indentStack[:len(p.indentStack)-1]
}

// Parse an indented block of code.
func (p *Parser) parseCodeBlock() (*CodeBlock, error) {
	indent, err := p.expectNewIndent()
	if err != nil {
		return nil, err
	}

	result := &CodeBlock{Labels: map[string]*LabelStmt{}}
	p.putBack(indent)

	p.identStack.pushScope()
	defer p.identStack.popScope()

	p.pushNewBranchStmtsTree()
	defer p.popBranchStmtsTree()

	for t := p.nextToken(); t.Type != TOKEN_EOF; t = p.nextToken() {
		p.putBack(t) // So that we can use handleIndentEnd
		end, err := p.handleIndentEnd()
		if err != nil {
			return nil, err
		}
		if end {
			break
		}

		stmt, err := p.parseStmt()
		if err != nil {
			return nil, err
		}

		if lbl, ok := stmt.(*LabelStmt); ok {
			if err := result.AddLabel(lbl); err != nil {
				return nil, err
			}
		}

		result.Statements = append(result.Statements, stmt)
	}

	p.topBranchStmtsTree().MatchGotoLabels(result.Labels)

	return result, nil
}

// Check if token `forWhat` is present before `untilWhat.
// It restores intitial state of the parser before returning.
func (p *Parser) scanForToken(forWhat, untilWhat TokenType) bool {
	nopeStack := []*Token{}
	token := p.nextToken()
	semicolon := false
	for token.Type != untilWhat && token.Type != TOKEN_EOF {
		if token.Type == forWhat {
			semicolon = true
			break
		}
		nopeStack = append(nopeStack, token)
		token = p.nextToken()
	}
	nopeStack = append(nopeStack, token)

	p.putBackStack(nopeStack)
	return semicolon
}

// Scan for ";" to see which version of statement will be parsed.
// For 'if' that can mean if there's a scoped variable declaration,
// for 'for' that can be a range/foreach loop or 3-expression one.
// We could also always initially assume scoped variable and backtrack
// on parse error, but this seems simpler.
// It restores intitial state of the parser before returning.
func (p *Parser) scanForSemicolon() bool {
	return p.scanForToken(TOKEN_SEMICOLON, TOKEN_COLON)
}

// Expects the keyword "for" to be already consumed.
func (p *Parser) parse3ClauseForStmt() (*ForStmt, error) {
	var err error

	result := ForStmt{}

	if p.peek().Type != TOKEN_SEMICOLON {
		p.identStack.pushScope()
		defer p.identStack.popScope()

		result.ScopedVarDecl, err = p.parseVarStmt(false)
		if err != nil {
			return nil, err
		}
	}

	// Consume first semicolon
	if t := p.expect(TOKEN_SEMICOLON); t == nil {
		return nil, fmt.Errorf("Expected semicolon")
	}

	if p.peek().Type != TOKEN_SEMICOLON {
		result.Condition, err = p.parseExpr()
		if err != nil {
			return nil, err
		}
	}

	// Consume second semicolon
	if t := p.expect(TOKEN_SEMICOLON); t == nil {
		return nil, fmt.Errorf("Expected semicolon")
	}

	if p.peek().Type != TOKEN_COLON {
		result.RepeatStmt, err = p.parseSimpleStmt(false)
		if err != nil {
			return nil, err
		}
	}

	// Consume the colon
	if t := p.expect(TOKEN_COLON); t == nil {
		return nil, fmt.Errorf("Expected `:` at the end of `for` statement")
	}

	result.Code, err = p.parseCodeBlock()
	if err != nil {
		return nil, err
	}

	return &result, nil
}

func (p *Parser) parseForStmt(lbl *LabelStmt) (stmt *ForStmt, err error) {
	ident := p.expect(TOKEN_FOR)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}

	// We push another BranchStmtsTree so that code like below fails:
	//  if true:
	//      break
	//  for x = 0; x < 10; x += 1:
	//      pass
	// Without this extra tree, for's MatchBranchableStmt would be called
	// for the surrounding block's tree. Another option would be to plug
	// it into for's CodeBlock, but it would result in nasty code.
	p.pushNewBranchStmtsTree()
	defer p.popBranchStmtsTree()

	threeClause := p.scanForSemicolon()

	if threeClause {
		stmt, err = p.parse3ClauseForStmt()
		if err != nil {
			return
		}
	} else {
		panic("todo")
	}

	p.topBranchStmtsTree().MatchBranchableStmt(stmt, "", TOKEN_BREAK, TOKEN_CONTINUE)
	if lbl != nil {
		p.topBranchStmtsTree().MatchBranchableStmt(stmt, lbl.Name(), TOKEN_BREAK, TOKEN_CONTINUE)
	}

	return
}

func (p *Parser) parseIf() (*IfStmt, error) {
	ident := p.expect(TOKEN_IF)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}

	scopedVar := p.scanForSemicolon()

	var (
		err           error
		scopedVarDecl *VarStmt = nil
	)

	if scopedVar {
		p.identStack.pushScope()
		defer p.identStack.popScope()

		scopedVarDecl, err = p.parseVarStmt(false)
		if err != nil {
			return nil, err
		}

		scolon := p.expect(TOKEN_SEMICOLON)
		if scolon == nil {
			return nil, fmt.Errorf("`;` expected")
		}
	}

	getCondAndBlock := func() (condition Expr, block *CodeBlock, err error) {
		condition, err = p.parseExpr()
		if err != nil {
			return nil, nil, fmt.Errorf("Couldn't parse the condition expression: %s", err)
		}

		colon := p.expect(TOKEN_COLON)
		if colon == nil {
			return nil, nil, fmt.Errorf("Expected `:` at the end of `if` condition")
		}

		block, err = p.parseCodeBlock()
		if err != nil {
			return nil, nil, err
		}
		return
	}

	condition, block, err := getCondAndBlock()
	if err != nil {
		return nil, err
	}

	branches := []*IfBranch{
		&IfBranch{
			stmt{expr: expr{ident.Offset}},
			scopedVarDecl,
			condition,
			block,
		}}

loop:
	for {
		isBranch, t := p.checkForBranch(TOKEN_ELIF, TOKEN_ELSE)
		if !isBranch {
			break loop
		}

		switch t.Type {
		case TOKEN_ELIF:
			condition, block, err := getCondAndBlock()
			if err != nil {
				return nil, err
			}
			branches = append(branches, &IfBranch{
				stmt{expr: expr{t.Offset}},
				nil,
				condition,
				block,
			})
		case TOKEN_ELSE:
			if colon := p.expect(TOKEN_COLON); colon == nil {
				return nil, fmt.Errorf("Expected `:` after `else`")
			}
			block, err := p.parseCodeBlock()
			if err != nil {
				return nil, err
			}
			branches = append(branches, &IfBranch{
				stmt{expr: expr{t.Offset}},
				nil,
				nil,
				block,
			})
			break loop
		default:
			p.putBack(t)
			break loop
		}
	}

	// TODO: else, elsif statements

	return &IfStmt{
		stmt{expr: expr{ident.Offset}},
		branches,
	}, nil
}

func (p *Parser) loadBuiltinFuncs() {
	for _, code := range builtinFuncs {
		parser := NewParserWithoutBuiltins(NewLexer([]rune(code)))
		fun, err := parser.parseFunc()
		if err != nil {
			panic(err)
		}

		decl := &Variable{name: fun.name, Type: fun.typ}
		p.identStack.addObject(decl)
	}
}

func (p *Parser) parseFuncStmt() (*VarStmt, error) {
	ident := p.expect(TOKEN_FUNC)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}

	if p.peek().Type == TOKEN_MUL {
		return nil, fmt.Errorf("Declared a non-method function as having a pointer receiver")
	}

	p.putBack(ident)

	fun, err := p.parseFunc()
	if err != nil {
		return nil, err
	}

	funcVar := &Variable{name: fun.name, Type: fun.typ}
	decl := &VarDecl{Vars: []*Variable{funcVar}, Inits: []Expr{fun}}

	// TODO: mark as final/not changeable
	p.identStack.addObject(funcVar)
	return &VarStmt{stmt{expr: expr{ident.Offset}}, []*VarDecl{decl}, true}, nil
}

// varKeyword controls whether the `var` keyword should be expected
// at the beginning.
func (p *Parser) parseVarStmt(varKeyword bool) (*VarStmt, error) {
	firstTok := p.nextToken()
	if varKeyword {
		if firstTok.Type != TOKEN_VAR {
			return nil, fmt.Errorf("Impossible happened")
		}
	} else {
		// We've just consumed part of the declaration, put it back.
		p.putBack(firstTok)
	}

	vars, err := p.parseVarDecl()
	if err != nil {
		return nil, err
	}

	stmt := &VarStmt{stmt{expr: expr{firstTok.Offset}}, vars, false}

	stmt.Vars.eachPair(func(v *Variable, init Expr) {
		p.identStack.addObject(v)
	})

	return stmt, nil
}

func (p *Parser) parseVarDecl() ([]*VarDecl, error) {
	unknownType := &UnknownType{}
	var varDecls = []*VarDecl{}
	var err error

	// The outermost loop iterates over groups of vars that are
	// initialized separately. For example, this:
	//    var x, y int = (1, 2), z = 3
	// would be handled in two steps, one for x, y, and one for z.
groupsLoop:
	for {
		// Parse left side of "="
		vars := []*Variable{}
	loop:
		for {
			decl := &Variable{Type: unknownType}

			token := p.nextToken()
			switch token.Type {
			case TOKEN_WORD:
				decl.name = token.Value.(string)
			case TOKEN_ASSIGN:
				break loop
			default:
				return nil, fmt.Errorf("Unexpected token %s\n", token.Type)
			}

			vars = append(vars, decl)
			token = p.nextToken()

			if token.Type != TOKEN_COMMA && token.Type != TOKEN_ASSIGN {
				// Type is specified, not inferred.
				p.putBack(token)
				decl.Type, err = p.parseType()
				if err != nil {
					return nil, err
				}
				// We have a type decl, it refers to all earlier declarations
				// without a type.
				for i := 0; i < len(vars); i++ {
					vars[i].Type = decl.Type
				}
				break loop
			}

			switch token.Type {
			case TOKEN_COMMA:
			case TOKEN_ASSIGN:
				p.putBack(token)
				break loop
			default:
				return nil, fmt.Errorf("Unexpected token %s", token.Type)
			}
		}

		// Right side of "="
		if len(vars) == 0 {
			return nil, fmt.Errorf("No vars declared on the left side of \"=\"")
		}

		switch t := p.nextToken(); t.Type {
		case TOKEN_COMMA:
			varDecls = append(varDecls, &VarDecl{Vars: vars})
			continue groupsLoop
		case TOKEN_ASSIGN:
			// Go on
		case TOKEN_EOF, TOKEN_INDENT, TOKEN_SEMICOLON, TOKEN_RPARENTH:
			p.putBack(t)
			// All default values.
			//for _, v := range vars {
			//	v.Init = nil
			//}
			varDecls = append(varDecls, &VarDecl{Vars: vars})
			break groupsLoop
		default:
			return nil, fmt.Errorf("Unexpected token after new vars list: %#v", t)
		}

		var inits []Expr
		varDecls = append(varDecls, &VarDecl{Vars: vars})

		// Parse a list of initializers in parentheses.
		if t := p.nextToken(); t.Type == TOKEN_LPARENTH {
			inits, err = p.parseArgs(0)
			if err != nil {
				return nil, err
			}

			if len(inits) == len(vars) {
				// Cool, it really was a list of initializers in parentheses.
				if t := p.expect(TOKEN_RPARENTH); t == nil {
					return nil, fmt.Errorf("Expected `)`")
				}
			} else if len(inits) == 1 {
				// Whoops, someone just put an expression in parentheses and we
				// treated it like a tuple. We need to fix this.
				if t := p.expect(TOKEN_RPARENTH); t == nil {
					return nil, fmt.Errorf("Expected `)`")
				}
				if t := p.nextToken(); t.Type == TOKEN_COMMA {
					restInits, err := p.parseArgs(0)
					if err != nil {
						return nil, err
					}
					inits = append(inits, restInits...)
				} else {
					p.putBack(t)
				}
			} else {
				return nil, fmt.Errorf("Couldn't parse the list of initializers")
			}
		} else {
			p.putBack(t)
			inits, err = p.parseArgs(len(vars))
			if err != nil {
				return nil, err
			}
		}

		if len(inits) != len(vars) && len(inits) != 1 {
			return nil, fmt.Errorf("Different number of new vars and initializers\n")
		}

		varDecls[len(varDecls)-1].Inits = inits

		if t := p.nextToken(); t.Type != TOKEN_COMMA {
			p.putBack(t)
			break groupsLoop
		}
	}
	return varDecls, nil
}

func (p *Parser) parseCompoundLit() (*CompoundLit, error) {
	startTok := p.expect(TOKEN_LBRACE)
	if startTok == nil {
		return nil, fmt.Errorf("Compound literal has to start with `{`")
	}

	p.skipWhiteSpace()

	if t := p.nextToken(); t.Type == TOKEN_RBRACE {
		return &CompoundLit{expr: expr{startTok.Offset}, typ: &UnknownType{}, kind: COMPOUND_EMPTY, elems: nil, contentPos: startTok.Offset}, nil
	} else {
		p.putBack(t)
	}

	kind := COMPOUND_UNKNOWN
	elems := []Expr{}

	for i := 0; true; i++ {
		p.skipWhiteSpace()

		// FIXME: ideas to do it concisely without parser-wide variables?
		p.ignoreUnknowns = true
		el, err := p.parseExpr()
		p.ignoreUnknowns = false
		if err != nil {
			return nil, err
		}

		p.skipWhiteSpace()

		elems = append(elems, el)

		if i%2 == 0 {
			switch t := p.nextToken(); t.Type {
			case TOKEN_COLON:
				if kind == COMPOUND_LISTLIKE {
					return nil, fmt.Errorf("Mixture of value and key:value expressions in a literal")
				}
				kind = COMPOUND_MAPLIKE
			case TOKEN_COMMA:
				if kind == COMPOUND_MAPLIKE {
					return nil, fmt.Errorf("Mixture of value and key:value expressions in a literal")
				}
				kind = COMPOUND_LISTLIKE
			case TOKEN_RBRACE:
				if kind == COMPOUND_MAPLIKE {
					return nil, fmt.Errorf("Unexpected end of a map-like compound literal")
				} else if kind == COMPOUND_UNKNOWN {
					kind = COMPOUND_LISTLIKE
				}
				return &CompoundLit{expr{startTok.Offset}, &UnknownType{}, kind, elems, startTok.Offset}, nil
			default:
				return nil, fmt.Errorf("Unexpected token in a compound literal")
			}
		} else {
			switch t := p.nextToken(); t.Type {
			case TOKEN_COMMA:
			case TOKEN_RBRACE:
				return &CompoundLit{expr{startTok.Offset}, &UnknownType{}, kind, elems, startTok.Offset}, nil
			default:
				return nil, fmt.Errorf("Unexpected token in a compound literal")
			}
		}
	}
	return nil, fmt.Errorf("Impossible happened")
}

func (p *Parser) parseStruct(receiverTypeDecl *TypeDecl) (*StructType, error) {
	name := ""
	if receiverTypeDecl != nil {
		// For class-like (with methods) struct declarations we need to get
		// TypeDecl (incomplete at this stage) of the struct being parsed.
		// It is needed for `self` variable.

		tokens, ok := p.expectSeries(TOKEN_STRUCT, TOKEN_WORD, TOKEN_COLON)
		if !ok {
			return nil, fmt.Errorf("Couldn't parse struct header")
		}
		name = tokens[1].Value.(string)
	} else {
		if _, ok := p.expectSeries(TOKEN_STRUCT, TOKEN_COLON); !ok {
			return nil, fmt.Errorf("Couldn't parse struct declaration")
		}
	}

	_, err := p.expectNewIndent()
	if err != nil {
		return nil, err
	}

	result := &StructType{Name: name, Members: map[string]Type{}, Keys: []string{}, Methods: map[string]*FuncDecl{}}

	selfType := &CustomType{Name: name, Decl: receiverTypeDecl}
	self, selfp := &Variable{name: "self", Type: selfType}, &Variable{name: "self", Type: &PointerType{To: selfType}}

	for {
		token := p.nextToken()

		switch token.Type {
		case TOKEN_WORD:
			name := token.Value.(string)
			typ, err := p.parseType()
			if err != nil {
				return nil, err
			}
			result.Members[name] = typ
			result.Keys = append(result.Keys, name)
		case TOKEN_INDENT:
			p.putBack(token)
			end, err := p.handleIndentEndOrNoToken(TOKEN_WORD, TOKEN_FUNC)
			if err != nil {
				return nil, err
			}
			if end {
				return result, nil
			}
			// Struct continues.
		case TOKEN_FUNC:
			if receiverTypeDecl == nil {
				return nil, fmt.Errorf("Cannot declare methods in inline struct declarations")
			}

			p.identStack.pushScope()

			receiver, ptrReceiver := self, false
			if p.peek().Type == TOKEN_MUL {
				receiver, ptrReceiver = selfp, true
			}

			p.identStack.addObject(receiver)

			p.putBack(token)
			fun, err := p.parseFunc()
			if err != nil {
				return nil, err
			}
			fun.Receiver, fun.PtrReceiver = receiver, ptrReceiver
			result.Methods[fun.name] = fun
			result.Keys = append(result.Keys, fun.name)
			p.identStack.popScope()
		default:
			p.putBack(token)
			p.forceIndentEnd()
			return result, nil
		}
	}
}

func (p *Parser) parseInterface(named bool) (*IfaceType, error) {
	name := ""
	if named {
		// For class-like (with methods) struct declarations we need to get
		// TypeDecl (incomplete at this stage) of the struct being parsed.
		// It is needed for `self` variable.

		tokens, ok := p.expectSeries(TOKEN_INTERFACE, TOKEN_WORD, TOKEN_COLON)
		if !ok {
			return nil, fmt.Errorf("Couldn't parse struct header")
		}
		name = tokens[1].Value.(string)
	} else {
		if _, ok := p.expectSeries(TOKEN_INTERFACE, TOKEN_COLON); !ok {
			return nil, fmt.Errorf("Couldn't parse struct declaration")
		}
	}

	_, err := p.expectNewIndent()
	if err != nil {
		return nil, err
	}

	result := &IfaceType{name: name, Keys: []string{}, Methods: map[string]*FuncDecl{}}

	for {
		token := p.nextToken()

		switch token.Type {
		case TOKEN_INDENT:
			p.putBack(token)
			end, err := p.handleIndentEndOrNoToken(TOKEN_FUNC)
			if err != nil {
				return nil, err
			}
			if end {
				return result, nil
			}
			// Interface continues.
		case TOKEN_FUNC:
			ptrReceiver := false
			if p.peek().Type == TOKEN_MUL {
				ptrReceiver = true
			}
			p.putBack(token)
			fun, err := p.parseFuncHeader()
			if err != nil {
				return nil, err
			}
			fun.PtrReceiver = ptrReceiver
			result.Methods[fun.name] = fun
			result.Keys = append(result.Keys, fun.name)
		default:
			p.putBack(token)
			p.forceIndentEnd()
			return result, nil
		}
	}
}

func (p *Parser) parseChanType() (*ChanType, error) {
	dir := CHAN_DIR_BI

	if p.peek().Type == TOKEN_SEND {
		dir = CHAN_DIR_RECEIVE
		p.nextToken()
	}

	if t := p.expect(TOKEN_CHAN); t == nil {
		return nil, fmt.Errorf("Expected `chan` after `<-`")
	}

	if p.peek().Type == TOKEN_SEND {
		if dir != CHAN_DIR_BI {
			return nil, fmt.Errorf("Invalid channel declaration")
		}

		dir = CHAN_DIR_SEND
		p.nextToken()
	}

	typ, err := p.parseType()
	if err != nil {
		return nil, err
	}

	return &ChanType{Of: typ, Dir: dir}, nil
}

func (p *Parser) parseType() (Type, error) {
	token := p.nextToken()
	switch token.Type {
	case TOKEN_MUL:
		ptrTo, err := p.parseType()
		if err != nil {
			return nil, err
		}
		return &PointerType{ptrTo}, nil
	case TOKEN_MAP:
		if p.expect(TOKEN_LBRACKET) == nil {
			return nil, fmt.Errorf("Expected `[` after `map`")
		}

		by, err := p.parseType()
		if err != nil {
			return nil, fmt.Errorf("Failed parsing map index type: %s", err)
		}

		if p.expect(TOKEN_RBRACKET) == nil {
			return nil, fmt.Errorf("Expected `]` after map's index type")
		}

		of, err := p.parseType()
		if err != nil {
			return nil, fmt.Errorf("Failed parsing map value type: %s", err)
		}

		return &MapType{by, of}, nil
	case TOKEN_LBRACKET:
		next := p.nextToken()
		switch next.Type {
		case TOKEN_RBRACKET:
			sliceOf, err := p.parseType()
			if err != nil {
				return nil, err
			}
			return &SliceType{sliceOf}, nil
		case TOKEN_INT:
			if p.expect(TOKEN_RBRACKET) == nil {
				// TODO: add location info
				return nil, fmt.Errorf("Expected ']'")
			}

			size, err := strconv.ParseInt(next.Value.(string), 10, 64)
			if err != nil {
				// TODO: add location info
				return nil, fmt.Errorf("Couldn't parse array size")
			}

			arrayOf, err := p.parseType()
			if err != nil {
				return nil, err
			}

			return &ArrayType{Of: arrayOf, Size: int(size)}, nil
		default:
			// TODO: add location info
			return nil, fmt.Errorf("Invalid type name, expected slice or array")

			// TODO:
			// case TOKEN_THREEDOTS
		}
	case TOKEN_WORD:
		name := token.Value.(string)
		var decl *TypeDecl = nil
		if !p.dontLookup {
			decl = p.identStack.findTypeDecl(name)
			if !p.ignoreUnknowns && decl == nil {
				return nil, fmt.Errorf("Type %s is unknown", name)
			}
			if decl.AliasedType == nil {
				return &SimpleType{ID: simpleTypeStrToID[name]}, nil
			}
			return &CustomType{Name: name, Decl: decl}, nil
		} else {
			// TODO: we don't want so much code which is mostly used just for tests
			if _, ok := GetBuiltinType(name); ok {
				return &SimpleType{ID: simpleTypeStrToID[name]}, nil
			}
			return &CustomType{Name: name, Decl: decl}, nil
		}
	case TOKEN_STRUCT:
		p.putBack(token)
		return p.parseStruct(nil)
	case TOKEN_INTERFACE:
		p.putBack(token)
		return p.parseInterface(false)
	case TOKEN_CHAN, TOKEN_SEND:
		p.putBack(token)
		return p.parseChanType()
	default:
		// TODO add location info
		return nil, fmt.Errorf("Expected type name, got %s", spew.Sdump(token))
	}
}

func (p *Parser) parseTypeExpr() (*TypeExpr, error) {
	token := p.nextToken()
	loc := token.Offset
	p.putBack(token)

	typ, err := p.parseType()
	if err != nil {
		return nil, err
	}

	return &TypeExpr{expr{loc}, typ}, nil
}

func (p *Parser) parsePrimaryExpr() (PrimaryExpr, error) {
	token := p.nextToken()
	var left Expr
	var err error

	// Sometimes we know that one simple expression can't make a full primary expression,
	// for example when we load a type name it must be followed by something,
	// either a literal or an expression that will be type-converted.
	// In such situations, we should skip any indents in-between.
	needsMore := false

	switch token.Type {
	case TOKEN_LPARENTH:
		left, err = p.parseExpr()
		if err != nil {
			return nil, err
		}
		if p.expect(TOKEN_RPARENTH) == nil {
			return nil, fmt.Errorf("Expected closing `)`")
		}
	case TOKEN_WORD:
		name := token.Value.(string)
		ident := &Ident{expr: expr{token.Offset}, name: name}

		if !p.dontLookup {
			if v := p.identStack.findObject(name); v == nil && !p.ignoreUnknowns {
				return nil, fmt.Errorf("Unknown identifier: %s", name)
			} else {
				ident.object = v
			}
		}
		left = ident
	case TOKEN_STR:
		left = &BasicLit{expr{token.Offset}, nil, token}
	case TOKEN_INT, TOKEN_FLOAT, TOKEN_IMAG, TOKEN_TRUE, TOKEN_FALSE, TOKEN_RUNE:
		return &BasicLit{expr{token.Offset}, nil, token}, nil
	case TOKEN_NIL:
		return &NilExpr{}, nil
	case TOKEN_MAP, TOKEN_STRUCT, TOKEN_LBRACKET:
		p.putBack(token)
		left, err = p.parseTypeExpr()
		if err != nil {
			return nil, err
		}
		needsMore = true
	case TOKEN_LBRACE:
		// Untyped compound literal, we'll have to deduce its type.
		p.putBack(token)
		left, err = p.parseCompoundLit()
		if err != nil {
			return nil, err
		}
	default:
		return nil, fmt.Errorf("Unexpected token (expected a primary expression): %s", token.Type)
	}

loop:
	for {
		token = p.nextToken()
		switch token.Type {
		case TOKEN_DOT:
			// TODO: parse type assertions
			selector := p.expect(TOKEN_WORD)
			left = &DotSelector{expr{token.Offset}, left, &Ident{expr{selector.Offset}, selector.Value.(string), nil}}
		case TOKEN_LPARENTH:
			args, err := p.parseArgs(0)
			if err != nil {
				return nil, err
			}
			if p.expect(TOKEN_RPARENTH) == nil {
				return nil, fmt.Errorf("Expected `)`")
			}
			left = &FuncCallExpr{expr{token.Offset}, left, args}
		case TOKEN_LBRACKET:
			index, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			if p.peek().Type == TOKEN_COLON {
				p.nextToken()

				from := index
				to, err := p.parseExpr()
				if err != nil {
					return nil, err
				}

				index = &SliceExpr{expr: expr{index.Pos()}, From: from, To: to}
			}
			if p.expect(TOKEN_RBRACKET) == nil {
				return nil, fmt.Errorf("Expected `]`")
			}
			left = &ArrayExpr{expr{token.Offset}, left, index, nil}
		case TOKEN_LBRACE:
			p.putBack(token)
			literal, err := p.parseCompoundLit()
			if err != nil {
				return nil, err
			}

			switch t := left.(type) {
			case *Ident:
				if p.dontLookup {
					literal.typ = &CustomType{Name: t.name}
				} else {
					if t.object.ObjectType() != OBJECT_TYPE {
						return nil, fmt.Errorf("Literal of non-typename expression `%s`", t.Type())
					}
					literal.typ = t.object.(*TypeDecl).Type()
				}
			case *TypeExpr:
				literal.typ = t.typ
			// TODO: DotSelector for types from other packages
			default:
				return nil, fmt.Errorf("Compound literal preceded with something that can't be a type: %T", t)
			}
			literal.updatePosWithType(left)
			left = literal
		case TOKEN_INDENT:
			if !needsMore {
				// Effectively a fallthrough
				p.putBack(token)
				break loop
			}
		default:
			p.putBack(token)
			break loop
		}
		// Something was just loaded.
		needsMore = false
	}

	return left, nil
}

// Return primary expression, possibly wrapped in an unary operator
func (p *Parser) parseMaybeUnaryExpr() (Expr, error) {
	token := p.nextToken()
	isOp, _ := opSet[token.Type] // FIXME we should create another set with just unary operators
	if isOp || token.Type == TOKEN_SEND {
		primaryExpr, err := p.parseMaybeUnaryExpr()
		if err != nil {
			return nil, err
		}
		return &UnaryOp{op: token, Right: primaryExpr}, nil
	} else {
		p.putBack(token)
		return p.parsePrimaryExpr()
	}
}

var hierarchy [][]TokenType = [][]TokenType{
	{TOKEN_MUL, TOKEN_DIV, TOKEN_AMP, TOKEN_PERCENT},
	{TOKEN_PLUS, TOKEN_MINUS, TOKEN_PIPE},
	{TOKEN_SHL, TOKEN_SHR},
	{TOKEN_LT, TOKEN_GT, TOKEN_EQ_GT, TOKEN_EQ_LT},
	{TOKEN_EQUALS},
	{TOKEN_OR, TOKEN_AND}}

var opSet map[TokenType]bool = make(map[TokenType]bool)

func init() {
	for _, layer := range hierarchy {
		for _, op := range layer {
			opSet[op] = true
		}
	}
}

func hierarchyNum(typ TokenType) int {
	for i, layer := range hierarchy {
		for _, t := range layer {
			if t == typ {
				return i
			}
		}
	}
	panic(fmt.Errorf("Token %#v isn't a binary operator", typ))
}

func (p *Parser) parseExpr() (Expr, error) {
	exprStack := []Expr{}
	opStack := []*Token{}

	reduce := func() {
		op := opStack[len(opStack)-1]
		reduced := &BinaryOp{
			expr:  expr{op.Offset},
			Left:  exprStack[len(exprStack)-2],
			Right: exprStack[len(exprStack)-1],
			op:    op}
		exprStack = append(exprStack[:len(exprStack)-2], reduced)
		opStack = opStack[:len(opStack)-1]
	}

	for {
		expr, err := p.parseMaybeUnaryExpr()
		if err != nil {
			return nil, err
		}
		exprStack = append(exprStack, expr)

		op := p.nextToken()
		isOp, _ := opSet[op.Type]
		if isOp {
			layer := hierarchyNum(op.Type)
			for len(opStack) > 0 && hierarchyNum(opStack[len(opStack)-1].Type) < layer {
				reduce()
			}
			opStack = append(opStack, op)
		} else {
			// Not an operator, so the expression ends here.
			p.putBack(op)
			// All operators left should now be ordered by their precedence (from the least
			// to the highest), so we can just reduce them all.
			for len(exprStack) > 1 {
				reduce()
			}
			return exprStack[0], nil
		}
	}
}

// Use max=0 for unbounded number of arguments.
func (p *Parser) parseArgs(max int) ([]Expr, error) {
	result := []Expr{}
	for {
		token := p.nextToken()
		switch token.Type {
		case TOKEN_EOF, TOKEN_RPARENTH, TOKEN_INDENT, TOKEN_SEMICOLON:
			p.putBack(token)
			return result, nil
		case TOKEN_COMMA:
			// nada
		default:
			p.putBack(token)
			expr, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			result = append(result, expr)
			if max > 0 && len(result) == max {
				return result, nil
			}
		}
	}
}

func (p *Parser) parseArgsDecl() (DeclChain, error) {
	// TODO: check default values are set only for parameters at the end
	t := p.nextToken()
	p.putBack(t)
	if t.Type == TOKEN_RPARENTH {
		return nil, nil
	}
	return p.parseVarDecl()
}

func (p *Parser) parseResultDecl() (DeclChain, error) {
	t := p.nextToken()
	if t.Type == TOKEN_LPARENTH {
		result, err := p.parseVarDecl()
		if err != nil {
			return nil, err
		}
		if t := p.expect(TOKEN_RPARENTH); t == nil {
			return nil, fmt.Errorf("Expected `)`")
		}
		return result, err
	} else {
		vars := []*Variable{}

		if t.Type == TOKEN_COLON {
			return []*VarDecl{&VarDecl{Vars: vars}}, nil
		}

		p.putBack(t)

	loop:
		for {
			typ, err := p.parseType()
			if err != nil {
				return nil, err
			}
			vars = append(vars, &Variable{
				name: "",
				Type: typ,
			})

			switch t := p.nextToken(); t.Type {
			case TOKEN_COMMA:
				// Go on
			case TOKEN_COLON, TOKEN_INDENT:
				p.putBack(t)
				break loop
			default:
				return nil, fmt.Errorf("Unexpected token %s", t.Type)
			}
		}
		return []*VarDecl{&VarDecl{Vars: vars}}, nil
	}
}

func typesFromVars(vd DeclChain) []Type {
	result := make([]Type, vd.countVars())
	i := 0
	vd.eachPair(func(d *Variable, init Expr) {
		result[i] = d.Type
		i++
	})
	return result
}

// Parses function header (declaration without the body).
// Returns a partially complete FuncDecl, that can be later filled with
// function's body, etc.
func (p *Parser) parseFuncHeader() (*FuncDecl, error) {
	startTok := p.expect(TOKEN_FUNC)
	if startTok == nil {
		return nil, fmt.Errorf("Function declaration needs to start with 'func' keyword")
	}

	if p.peek().Type == TOKEN_MUL {
		p.nextToken()
	}

	funcName := ""
	t := p.nextToken()
	switch t.Type {
	case TOKEN_WORD:
		funcName = t.Value.(string)
	case TOKEN_LPARENTH:
		// anonymous function
		p.putBack(t)
	default:
		return nil, fmt.Errorf("Unexpected token after `func`: %#v", t.Type)
	}

	if t := p.expect(TOKEN_LPARENTH); t == nil {
		return nil, fmt.Errorf("Expected `(`")
	}

	args, err := p.parseArgsDecl()
	if err != nil {
		return nil, err
	}

	if t := p.expect(TOKEN_RPARENTH); t == nil {
		return nil, fmt.Errorf("Expected `)`")
	}

	results := DeclChain(nil)

	// Check if ':' is next - if so, function doesn't return anything.
	t = p.peek()
	if t.Type != TOKEN_COLON && t.Type != TOKEN_INDENT {
		results, err = p.parseResultDecl()
		if err != nil {
			return nil, err
		}
	}

	return &FuncDecl{
		expr:    expr{startTok.Offset},
		name:    funcName,
		Args:    args,
		Results: results,
		typ: &FuncType{
			Args:    typesFromVars(args),
			Results: typesFromVars(results),
		},
	}, nil
}

func (p *Parser) parseFunc() (*FuncDecl, error) {
	fd, err := p.parseFuncHeader()
	if err != nil {
		return nil, err
	}

	if t := p.expect(TOKEN_COLON); t == nil {
		return nil, fmt.Errorf("Expected `:`")
	}

	p.identStack.pushScope()
	defer p.identStack.popScope()

	// Make arguments accessiable within the function body.
	fd.Args.eachPair(func(arg *Variable, init Expr) {
		p.identStack.addObject(arg)
	})

	// Make named results accessiable within the function body.
	fd.Results.eachPair(func(r *Variable, init Expr) {
		p.identStack.addObject(r)
	})

	block, err := p.parseCodeBlock()
	if err != nil {
		return nil, err
	}

	if p.topBranchStmtsTree().CountBranchStmts() > 0 {
		return nil, fmt.Errorf("Unmatched branch statements: %#v", p.topBranchStmtsTree())
	}

	fd.Code = block

	return fd, nil
}

func (p *Parser) parseTypeDecl() (*TypeDecl, error) {
	startTok := p.expect(TOKEN_TYPE)
	if startTok == nil {
		return nil, fmt.Errorf("Type declaration needs to start with 'type' keyword")
	}

	name := p.expect(TOKEN_WORD)
	if name == nil {
		return nil, fmt.Errorf("Type name expected")
	}

	realType, err := p.parseType()
	if err != nil {
		return nil, err
	}

	result := &TypeDecl{
		stmt:        stmt{expr: expr{startTok.Offset}},
		name:        name.Value.(string),
		AliasedType: realType,
	}
	p.identStack.addObject(result)
	return result, nil
}

func (p *Parser) parseBranchStmt() (*BranchStmt, error) {
	tok := p.nextToken()

	id := (*Ident)(nil)
	if p.peek().Type == TOKEN_WORD {
		word := p.nextToken()

		id = &Ident{expr{word.Offset}, word.Value.(string), nil}
		// TODO: lookup ident (when label parsing is implemented)
	}

	r := &BranchStmt{stmt{expr: expr{tok.Offset}}, tok, id, nil, nil}
	p.topBranchStmtsTree().Members.Add(r)

	return r, nil
}

func (p *Parser) parseExprList() ([]Expr, error) {
	result := []Expr{}
	for {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		result = append(result, expr)

		if p.peek().Type != TOKEN_COMMA {
			break
		}
		p.nextToken()
	}
	return result, nil
}

func (p *Parser) parseSimpleStmt(labelPossible bool) (SimpleStmt, error) {
	// We make an exception if the next token is TOKEN_COLON, because TOKEN_COLON means
	// that we're parsing a new label statement, so we don't want any ident lookups
	// (goto can jump forwards, which would result in unknown ident errors)
	if labelPossible {
		if tokens, ok := p.expectSeries(TOKEN_WORD, TOKEN_COLON); ok {
			name := tokens[0].Value.(string)
			return &LabelStmt{stmt: stmt{expr: expr{tokens[0].Offset}}, name: name}, nil
		}
	}

	lhs, err := p.parseExprList()
	if err != nil {
		return nil, err
	}

	firstTok := p.peek()

	switch firstTok.Type {
	case TOKEN_SEND:
		if len(lhs) > 1 {
			return nil, fmt.Errorf("More than one expression on the left side of the send expression")
		}

		p.nextToken()
		rhs, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		return &SendStmt{stmt{expr: expr{firstTok.Offset}}, lhs[0], rhs}, nil
	case TOKEN_PLUS_ASSIGN, TOKEN_MINUS_ASSIGN: // TODO: add other ops
		if len(lhs) > 1 {
			return nil, fmt.Errorf("More than one expression on the left side of assignment")
		}
		fallthrough
	case TOKEN_ASSIGN:
		p.nextToken()
		rhs, err := p.parseExprList()
		if err != nil {
			return nil, err
		}
		if len(lhs) != len(rhs) && len(rhs) != 1 {
			return nil, fmt.Errorf("Different number of values in assignment (%d and %d)", len(lhs), len(rhs))
		}
		return &AssignStmt{stmt{expr: expr{firstTok.Offset}}, lhs, rhs, firstTok}, nil
	}

	if len(lhs) > 1 {
		return nil, fmt.Errorf("Unexpected list of expressions")
	}

	if len(lhs) == 0 {
		return nil, nil
	}

	switch p.peek().Type {
	// TODO: parse sending to channels, increment/decrement statements, maybe short var declarations, etc
	default:
		return &ExprStmt{stmt{expr: expr{firstTok.Offset}}, lhs[0]}, nil
	}
}

func (p *Parser) parseStructStmt() (*StructStmt, error) {
	firstTok := p.peek()

	typeDecl := &TypeDecl{
		stmt: stmt{expr: expr{firstTok.Offset}},
	}

	structDecl, err := p.parseStruct(typeDecl)
	if err != nil {
		return nil, err
	}

	typeDecl.name = structDecl.Name
	typeDecl.AliasedType = structDecl
	typeDecl.Methods = structDecl.Methods

	p.identStack.addObject(typeDecl)

	return &StructStmt{stmt{expr: expr{firstTok.Offset}}, structDecl}, nil
}

func (p *Parser) parseIfaceStmt() (*IfaceStmt, error) {
	firstTok := p.peek()

	typeDecl := &TypeDecl{
		stmt: stmt{expr: expr{firstTok.Offset}},
	}

	ifaceDecl, err := p.parseInterface(true)
	if err != nil {
		return nil, err
	}

	typeDecl.name = ifaceDecl.name
	typeDecl.AliasedType = ifaceDecl
	typeDecl.Methods = ifaceDecl.Methods

	p.identStack.addObject(typeDecl)

	return &IfaceStmt{stmt{expr: expr{firstTok.Offset}}, ifaceDecl}, nil
}

func (p *Parser) parseStmt() (Stmt, error) {
	lbl := p.prevLbl
	p.prevLbl = nil
	for {
		token := p.nextToken()
		switch token.Type {
		case TOKEN_VAR:
			p.putBack(token)
			return p.parseVarStmt(true)
		case TOKEN_IF:
			p.putBack(token)
			return p.parseIf()
		case TOKEN_FOR:
			p.putBack(token)
			return p.parseForStmt(lbl)
		case TOKEN_FUNC:
			p.putBack(token)
			return p.parseFuncStmt()
		case TOKEN_TYPE:
			p.putBack(token)
			return p.parseTypeDecl()
		case TOKEN_INDENT:
			if token.Value.(string) != "" {
				return nil, fmt.Errorf("Unexpected indent, '%s'", token.Value.(string))
			}
		case TOKEN_PASS:
			return &PassStmt{stmt{expr: expr{token.Offset}}}, nil
		case TOKEN_GOTO, TOKEN_BREAK, TOKEN_CONTINUE, TOKEN_FALLTHROUGH:
			p.putBack(token)
			return p.parseBranchStmt()
		case TOKEN_EOF:
			return nil, nil
		case TOKEN_STRUCT:
			p.putBack(token)
			return p.parseStructStmt()
		case TOKEN_INTERFACE:
			p.putBack(token)
			return p.parseIfaceStmt()
		default:
			p.putBack(token)
			stmt, err := p.parseSimpleStmt(true)
			p.prevLbl, _ = stmt.(*LabelStmt)
			return stmt, err
		}
	}
}

type File struct {
	Pkg        string
	Statements []Stmt
}

func (p *Parser) ParseFile() (*File, error) {
	if p.expect(TOKEN_PACKAGE) == nil {
		return nil, fmt.Errorf("Expected keyword `package` at the beginning of a file")
	}

	pkg := ""
	if t := p.expect(TOKEN_WORD); t == nil {
		return nil, fmt.Errorf("Expected package name after the `package` keyword")
	} else {
		pkg = t.Value.(string)
	}

	stmts, err := p.Parse()
	if err != nil {
		return nil, err
	}

	return &File{Pkg: pkg, Statements: stmts}, nil
}

func (p *Parser) Parse() ([]Stmt, error) {
	var result = []Stmt{}
	for t := p.nextToken(); t.Type != TOKEN_EOF; t = p.nextToken() {
		p.putBack(t)
		stmt, err := p.parseStmt()
		if err != nil {
			return nil, err
		}
		if stmt == nil {
			// EOF
			break
		}
		result = append(result, stmt)
	}
	return result, nil
}
