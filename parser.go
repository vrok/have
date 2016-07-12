package have

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

type Parser struct {
	lex              *Lexer
	tokensBuf        []*Token
	indentStack      []string
	identStack       *IdentStack
	branchTreesStack BranchTreesStack
	funcStack        []*FuncDecl

	// TODO: Remove after implementing unboundVars
	ignoreUnknowns bool
	unboundTypes   map[string][]*CustomType
	unboundIdents  map[string][]*Ident
	topLevelDecls  map[string]Object

	imports Imports

	// genericParams and generic normally are nils, unless we're parsing a generic instantiation
	genericParams map[string]Type
	generic       Generic

	dontLookup bool

	prevLbl *LabelStmt // Just declared labal is stored here temporarily
}

type Imports map[string]*ImportStmt

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
	// Push a scope so that builtins are stored in a separate scope.
	parser.identStack.pushScope()
	return parser
}

func NewParserWithoutBuiltins(lex *Lexer) *Parser {
	return &Parser{lex: lex,
		identStack:       &IdentStack{map[string]Object{}},
		branchTreesStack: []*BranchStmtsTree{NewBranchStmtsTree()},
		unboundTypes:     make(map[string][]*CustomType),
		unboundIdents:    make(map[string][]*Ident),
		topLevelDecls:    make(map[string]Object),
		imports:          make(map[string]*ImportStmt)}
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
	token := p.nextToken()
	defer p.putBack(token)
	if token.Type != TOKEN_INDENT {
		return false, fmt.Errorf("Indent expected")
	}

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
// If parser is pointed to a token that is not indent, it will return
// true as well, this is useful in code like this:
//
// apply({1, 2, 3}, func(x int) int:
//     return x * 2) // <- block ended by ')'
func (p *Parser) handleIndentEnd() (end bool, err error) {
	end, err = p.isIndentEnd()
	if !end && p.peek().Type != TOKEN_INDENT {
		end, err = true, nil
	}

	if end {
		// Pop current indent
		p.indentStack = p.indentStack[:len(p.indentStack)-1]
		return
	}
	p.expect(TOKEN_INDENT)
	return
}

// This is very similar to handleIndentEnd, but the indented block
// of code can also be ended by an occurence of a token not in tokenTypes
// (not necessarily preceded by an end of indentation, or precended by an
// unmatched indent).
// Example:
//
// var y = struct:
//     x int
//      {x: 1}  // <- '{' ends the indented block of code
//
// Another one:
//
// var y = struct:
//     x int
//   {x: 1}  // <- unmatched indent, but it's all right
//
// The special character is put back to the tokenizer, so that things
// like compount initializers of nested structures work.
func (p *Parser) handleIndentEndOrNoToken(tokenTypes ...TokenType) (end bool, err error) {
	end, err = p.handleIndentEnd()
	if end {
		return end, err
	}
	next := p.peek()
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

// Return types of at most N next tokens. Can be fewer when there aren't enough tokens
// left.
func (p *Parser) peekN(n int) []TokenType {
	var result []TokenType
	var tokens []*Token

	for i := 0; i < n; i++ {
		t := p.nextToken()
		tokens = append(tokens, t)
		result = append(result, t.Type)
		if t.Type == TOKEN_EOF {
			break
		}
	}
	p.putBackStack(tokens)
	return result
}

func tokenTypesEq(a, b []TokenType) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
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

	p.branchTreesStack.pushNew()
	defer p.branchTreesStack.pop()

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

	p.branchTreesStack.top().MatchGotoLabels(result.Labels)

	return result, nil
}

// Check if token `forWhat` is present before `untilWhat`.
// It restores initial state of the parser before returning.
func (p *Parser) scanForToken(forWhat TokenType, untilWhat []TokenType) bool {
	nopeStack := []*Token{}
	token := p.nextToken()
	semicolon := false
	hit := func(typ TokenType) bool {
		for _, t := range untilWhat {
			if t == typ {
				return true
			}
		}
		return false
	}
	for !hit(token.Type) && token.Type != TOKEN_EOF {
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

// Scan for `;` to see which version of statement is being parsed.
// For `if` that tells us if there's a scoped variable declaration,
// for `for` that can be a range/foreach loop or a 3-expression one.
// We could also always initially assume scoped variable and backtrack
// on parse error, but this seems simpler.
// It restores initial state of the parser before returning.
func (p *Parser) scanForSemicolon() bool {
	return p.scanForToken(TOKEN_SEMICOLON, []TokenType{TOKEN_COLON})
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
	p.branchTreesStack.pushNew()
	defer p.branchTreesStack.pop()

	threeClause := p.scanForSemicolon()

	if threeClause {
		stmt, err = p.parse3ClauseForStmt()
		if err != nil {
			return
		}
	} else {
		panic("todo")
	}

	p.branchTreesStack.top().MatchBranchableStmt(stmt, "", TOKEN_BREAK, TOKEN_CONTINUE)
	if lbl != nil {
		p.branchTreesStack.top().MatchBranchableStmt(stmt, lbl.Name(), TOKEN_BREAK, TOKEN_CONTINUE)
	}

	return
}

func (p *Parser) parseColonWithCodeBlock() (*CodeBlock, error) {
	colon := p.expect(TOKEN_COLON)
	if colon == nil {
		return nil, fmt.Errorf("Expected `:` at the end of `if` condition")
	}

	return p.parseCodeBlock()
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

		block, err = p.parseColonWithCodeBlock()
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
			block, err := p.parseColonWithCodeBlock()
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

func (p *Parser) parseSwitchStmt() (*SwitchStmt, error) {
	ident := p.expect(TOKEN_SWITCH)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}

	//scopedVar := p.scanForSemicolon()
	scopedVar := p.scanForToken(TOKEN_SEMICOLON, []TokenType{TOKEN_CASE, TOKEN_DEFAULT})

	var (
		err                     error
		scopedVarStmt, mainStmt Stmt
		branches                []*SwitchBranch
	)

	if scopedVar {
		p.identStack.pushScope()
		defer p.identStack.popScope()

		scopedVarStmt, err = p.parseInitOrAssign()
		if err != nil {
			return nil, err
		}

		scolon := p.expect(TOKEN_SEMICOLON)
		if scolon == nil {
			return nil, fmt.Errorf("`;` expected")
		}
	}

	var typeSwitchVar *Variable

	switch p.peek().Type {
	case TOKEN_DEFAULT, TOKEN_CASE, TOKEN_INDENT:
		// No main stmt/expr
	case TOKEN_VAR:
		var varStmt *VarStmt
		// Push and pop scope so that this variable isn't available for binding,
		// we use its copies instead (type switches are a bit odd).
		p.identStack.pushScope()
		varStmt, err = p.parseVarStmt(true)
		p.identStack.popScope()
		if len(varStmt.Vars) != 1 || len(varStmt.Vars[0].Vars) != 1 {
			return nil, fmt.Errorf("Invalid variable declaration in switch header")
		}
		typeSwitchVar = varStmt.Vars[0].Vars[0]
		mainStmt = varStmt
	default:
		mainStmt, err = p.parseSimpleStmt(false)
	}

	if err != nil {
		return nil, err
	}

loop:
	for {
		isBranch, t := p.checkForBranch(TOKEN_CASE, TOKEN_DEFAULT)
		if !isBranch {
			break loop
		}

		switch t.Type {
		case TOKEN_CASE:
			val, err := p.parseExprList()
			if err != nil {
				return nil, err
			}

			// Scope just for easy disposal of typeSwitchVar.
			p.identStack.pushScope()

			var typeSwitchVarCopy *Variable
			if typeSwitchVar != nil {
				typeSwitchVarCopy = &(*typeSwitchVar)
				p.identStack.addObject(typeSwitchVarCopy)
			}

			block, err := p.parseColonWithCodeBlock()
			p.identStack.popScope()
			if err != nil {
				return nil, err
			}

			branches = append(branches, &SwitchBranch{
				stmt:          stmt{expr: expr{t.Offset}},
				Values:        val,
				Code:          block,
				TypeSwitchVar: typeSwitchVarCopy,
			})
		case TOKEN_DEFAULT:
			block, err := p.parseColonWithCodeBlock()
			if err != nil {
				return nil, err
			}

			branches = append(branches, &SwitchBranch{
				stmt: stmt{expr: expr{t.Offset}},
				Code: block,
			})
		default:
			p.putBack(t)
			break loop
		}
	}

	return &SwitchStmt{
		stmt{expr: expr{ident.Offset}},
		scopedVarStmt,
		mainStmt,
		branches,
	}, nil
}

func (p *Parser) loadBuiltinFuncs() {
	for _, code := range builtinFuncs {
		parser := NewParserWithoutBuiltins(NewLexer([]rune(code)))
		fun, _, err := parser.parseFunc(false)
		if err != nil {
			panic(err)
		}

		decl := &Variable{name: fun.name, Type: fun.typ}
		p.identStack.addObject(decl)
	}
}

func (p *Parser) parseFuncStmt() (Stmt, error) {
	ident := p.expect(TOKEN_FUNC)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}

	if p.peek().Type == TOKEN_MUL {
		return nil, fmt.Errorf("Declared a non-method function as having a pointer receiver")
	}

	p.putBack(ident)

	fun, obj, err := p.parseFunc(true)
	if err != nil {
		return nil, err
	}

	if len(fun.GenericParams) > 0 {
		// TODO: this is ugly
		return obj.(*GenericFunc), nil
	}

	funcVar := obj.(*Variable)
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
				return &CompoundLit{expr{startTok.Offset}, nil, &UnknownType{}, kind, elems, startTok.Offset}, nil
			default:
				return nil, fmt.Errorf("Unexpected token in a compound literal")
			}
		} else {
			switch t := p.nextToken(); t.Type {
			case TOKEN_COMMA:
			case TOKEN_RBRACE:
				return &CompoundLit{expr{startTok.Offset}, nil, &UnknownType{}, kind, elems, startTok.Offset}, nil
			default:
				return nil, fmt.Errorf("Unexpected token in a compound literal")
			}
		}
	}
	return nil, fmt.Errorf("Impossible happened")
}

func (p *Parser) parseStruct(receiverTypeDecl *TypeDecl, genericPossible bool) (*StructType, error) {
	name := ""

	var genericParams []string
	var err error

	if receiverTypeDecl != nil {
		// For class-like (with methods) struct declarations we need to get
		// TypeDecl (incomplete at this stage) of the struct being parsed.
		// It is needed for `self` variable.

		tokens, ok := p.expectSeries(TOKEN_STRUCT, TOKEN_WORD)
		if !ok {
			return nil, fmt.Errorf("Couldn't parse struct header")
		}
		name = tokens[1].Value.(string)

		switch t := p.peek(); t.Type {
		case TOKEN_LBRACKET:
			if !genericPossible {
				return nil, fmt.Errorf("Generic types can only be declared top-level")
			}
			// Scope for generic params
			p.identStack.pushScope()
			defer p.identStack.popScope()
			genericParams, err = p.parseGenericParams()
			if err != nil {
				return nil, err
			}

			if p.expect(TOKEN_COLON) == nil {
				return nil, fmt.Errorf("Expected `:` after `]`")
			}
		case TOKEN_COLON:
			p.nextToken()
		default:
			return nil, fmt.Errorf("Couldn't parse struct header")
		}
	} else {
		if _, ok := p.expectSeries(TOKEN_STRUCT, TOKEN_COLON); !ok {
			return nil, fmt.Errorf("Couldn't parse struct declaration")
		}
	}

	_, err = p.expectNewIndent()
	if err != nil {
		return nil, err
	}

	result := &StructType{Name: name, Members: map[string]Type{}, Keys: []string{}, Methods: map[string]*FuncDecl{}, GenericParams: genericParams}

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
			fun, _, err := p.parseFunc(false)
			if err != nil {
				return nil, err
			}
			fun.Receiver, fun.PtrReceiver = receiver, ptrReceiver
			result.Methods[fun.name] = fun
			result.Keys = append(result.Keys, fun.name)
			p.identStack.popScope()
		case TOKEN_PASS:
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
			fun, err := p.parseFuncHeader(false)
			if err != nil {
				return nil, err
			}
			fun.PtrReceiver = ptrReceiver
			result.Methods[fun.name] = fun
			result.Keys = append(result.Keys, fun.name)
		case TOKEN_PASS:
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

func (p *Parser) parseFuncType() (*FuncType, error) {
	hdr, err := p.parseFuncHeader(false)
	if err != nil {
		return nil, err
	}
	return hdr.typ, nil
}

// False when parsing in normal mode, true when parsing a generic
// instantiation/instantiation.
func (p *Parser) parsingGenericInstantiation() bool {
	return p.genericParams != nil
}

func (p *Parser) typeFromWord(name string) Type {
	if p.parsingGenericInstantiation() {
		// Substitute a generic param occurence with a concrete type.
		if typ, ok := p.genericParams[name]; ok {
			return typ
		}
	}

	if !p.dontLookup {
		obj := p.identStack.findTypeDecl(name)
		switch {
		case obj == nil:
			r := &CustomType{Name: name}
			p.unboundTypes[name] = append(p.unboundTypes[name], r)
			return r
		case obj.ObjectType() == OBJECT_TYPE:
			decl := obj.(*TypeDecl)
			if decl.AliasedType == nil {
				return &SimpleType{ID: simpleTypeStrToID[name]}
			} else {
				return &CustomType{Name: name, Decl: decl}
			}
		case obj.ObjectType() == OBJECT_GENERIC_TYPE:
			return &GenericParamType{Name: obj.Name()}
		default:
			panic("niemożliwe")
		}
	} else {
		// TODO: we don't want so much code which is mostly used just for tests
		if _, ok := GetBuiltinType(name); ok {
			return &SimpleType{ID: simpleTypeStrToID[name]}
		}
		return &CustomType{Name: name, Decl: nil}
	}
}

func (p *Parser) parseGenericParamTypes() ([]Type, error) {
	if p.expect(TOKEN_LBRACKET) == nil {
		return nil, fmt.Errorf("Expected `[`")
	}

	var result []Type
	for {
		typ, err := p.parseType()
		if err != nil {
			return nil, err
		}

		result = append(result, typ)

		switch p.nextToken().Type {
		case TOKEN_COMMA:
		case TOKEN_RBRACKET:
			return result, nil
		default:
			return nil, fmt.Errorf("Unexpected token")
		}
	}
}

func (p *Parser) parseType() (Type, error) {
	return p.attemptTypeParse(false)
}

var doesntLookLikeTypeErr = errors.New("Not a type")

// When justTry is false, it just parses a type.
// But when jutryTry is true, this function, besides parsing, can also be used
// to check if the next token could be the beginning of a type at all.
func (p *Parser) attemptTypeParse(justTry bool) (Type, error) {
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

		switch {
		case tokenTypesEq(p.peekN(3), []TokenType{TOKEN_DOT, TOKEN_WORD, TOKEN_LBRACKET}):
			// External generic type
			dot, memberName := p.nextToken(), p.nextToken()

			//pkg := &Ident{expr: expr{token.Offset}, name: name}
			pkg := p.wordToExpr(token)
			member := &Ident{expr: expr{memberName.Offset}, name: memberName.Value.(string)}

			// HERE
			left := &DotSelector{expr: expr{dot.Offset}, Left: pkg, Right: member}

			params, err := p.parseGenericParamTypes()
			if err != nil {
				return nil, err
			}
			return &GenericType{Left: left, Params: params}, nil
		case p.peek().Type == TOKEN_LBRACKET:
			// Local generic type
			//pkg := &Ident{expr: expr{token.Offset}, name: name}
			pkg := p.wordToExpr(token)
			params, err := p.parseGenericParamTypes()
			if err != nil {
				return nil, err
			}
			return &GenericType{Left: pkg, Params: params}, nil
		default:
			// Not a generic type
			if p.peek().Type == TOKEN_DOT {
				p.nextToken()
				membNameTok := p.nextToken()
				if membNameTok.Type != TOKEN_WORD {
					return nil, fmt.Errorf("Package member name expected after `.`")
				}
				membName := membNameTok.Value.(string)

				pkg, ok := p.imports[name]
				if !ok {
					return nil, fmt.Errorf("Package `%s` not imported", name)
				}

				fullName := name + "." + membName
				typ := &CustomType{Name: membName, Package: pkg}
				p.unboundTypes[fullName] = append(p.unboundTypes[fullName], typ)
				return typ, nil
			} else {
				return p.typeFromWord(name), nil
			}
		}
	case TOKEN_STRUCT:
		p.putBack(token)
		return p.parseStruct(nil, false)
	case TOKEN_INTERFACE:
		p.putBack(token)
		return p.parseInterface(false)
	case TOKEN_CHAN, TOKEN_SEND:
		p.putBack(token)
		return p.parseChanType()
	case TOKEN_FUNC:
		p.putBack(token)
		return p.parseFuncType()
	default:
		if justTry {
			p.putBack(token)
			return nil, doesntLookLikeTypeErr
		}
		// TODO add location info
		return nil, fmt.Errorf("Expected type name, got %s", token.Type)
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

// Parses either a function type name or a function literal. It's useful because they
// begin similarily and can both be used in primary expressions.
// Returns either TypeExpr with function type or FuncDecl.
func (p *Parser) parseFuncTypeOrLit() (Expr, error) {
	loc := p.peek().Offset
	fd, err := p.parseFuncHeader(false)
	if err != nil {
		return nil, err
	}

	if p.peek().Type == TOKEN_COLON {
		return p.parseFuncBody(fd)
	} else {
		return &TypeExpr{expr{loc}, fd.typ}, nil
	}
}

// word.Type must be TOKEN_WORD
func (p *Parser) wordToExpr(word *Token) PrimaryExpr {
	if word.Type != TOKEN_WORD {
		panic("wordToExpr: token is not a word")
	}
	name := word.Value.(string)
	ident := &Ident{expr: expr{word.Offset}, name: name}
	var result PrimaryExpr = ident

	if p.parsingGenericInstantiation() && p.genericParams[name] != nil {
		typ, ok := p.genericParams[name]
		if !ok {
			panic("Internal error")
		}
		result = &TypeExpr{expr: expr{word.Offset}, typ: typ}
	} else if !p.dontLookup {
		if v := p.identStack.findObject(name); v == nil && !p.ignoreUnknowns {
			if pkg := p.imports[name]; pkg == nil {
				p.unboundIdents[name] = append(p.unboundIdents[name], ident)
			} else {
				ident.object = pkg
			}
		} else {
			ident.object = v
		}
	}
	return result
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
		left = p.wordToExpr(token)
	case TOKEN_STR:
		left = &BasicLit{expr{token.Offset}, token}
	case TOKEN_INT, TOKEN_FLOAT, TOKEN_IMAG, TOKEN_TRUE, TOKEN_FALSE, TOKEN_RUNE:
		return &BasicLit{expr{token.Offset}, token}, nil
	case TOKEN_NIL:
		return &NilExpr{}, nil
	case TOKEN_FUNC:
		p.putBack(token)
		left, err = p.parseFuncTypeOrLit()
		if err != nil {
			return nil, err
		}
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
			switch t := p.nextToken(); t.Type {
			case TOKEN_LPARENTH:
				var te *TypeExpr
				if p.peek().Type == TOKEN_TYPE {
					p.nextToken()
				} else {
					te, err = p.parseTypeExpr()
					if err != nil {
						return nil, err
					}
				}
				if p.expect(TOKEN_RPARENTH) == nil {
					return nil, fmt.Errorf("Expected `)`")
				}
				return &TypeAssertion{expr{token.Offset}, te == nil, left, te}, nil
			case TOKEN_WORD:
				left = &DotSelector{expr{token.Offset}, left, &Ident{expr{t.Offset}, t.Value.(string), nil, false}}
			default:
				return nil, fmt.Errorf("Unexpected token after `.`")
			}
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
			var index []Expr
			exp, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			switch p.peek().Type {
			case TOKEN_COLON:
				p.nextToken()

				from := exp
				to, err := p.parseExpr()
				if err != nil {
					return nil, err
				}

				index = append(index, &SliceExpr{expr: expr{exp.Pos()}, From: from, To: to})
			case TOKEN_COMMA:
				index = append(index, exp)

				for p.peek().Type == TOKEN_COMMA {
					p.nextToken()
					exp, err := p.parseExpr()
					if err != nil {
						return nil, err
					}

					index = append(index, exp)
				}
			default:
				index = append(index, exp)
			}

			if p.expect(TOKEN_RBRACKET) == nil {
				return nil, fmt.Errorf("Expected `]`")
			}
			left = &ArrayExpr{expr{token.Offset}, left, index}
		case TOKEN_LBRACE:
			p.putBack(token)
			literal, err := p.parseCompoundLit()
			if err != nil {
				return nil, err
			}
			literal.Left = left
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

func (p *Parser) typesToVars(types []Type, varsType Type) ([]*Variable, error) {
	var result []*Variable
	for _, t := range types {
		switch ct := t.(type) {
		case *CustomType:
			result = append(result, &Variable{name: ct.Name, Type: varsType})
		default:
			return nil, fmt.Errorf("Invalid type name: %s", t)
		}
	}
	return result, nil
}

func (p *Parser) makeUnnamedVars(types []Type) []*Variable {
	var result []*Variable
	for _, t := range types {
		result = append(result, &Variable{Type: t})
	}
	return result
}

func (p *Parser) parseArgsDecl() (DeclChain, error) {
	if p.peek().Type == TOKEN_RPARENTH {
		return nil, nil
	}

	var result []*Variable
	var types []Type

	var names []*Token

	type knowledgeState int
	const undecided, named, anon knowledgeState = 0, 1, 2
	state := undecided

loop:
	for {
		switch state {
		case undecided:
			if p.peek().Type == TOKEN_WORD && !tokenTypesEq(p.peekN(3), []TokenType{TOKEN_WORD, TOKEN_DOT, TOKEN_WORD}) {
				// We don't know yet if it's a parameter name or type name.
				names = append(names, p.nextToken())
			} else {
				// This surely isn't a parameter name, but could be a type name.
				state = anon
				continue loop
			}
		case named:
			names = append(names, p.nextToken())
		case anon:
			t, err := p.parseType()
			if err != nil {
				return nil, err
			}
			types = append(types, t)
		}

		switch p.peek().Type {
		case TOKEN_RPARENTH, TOKEN_COLON, TOKEN_INDENT:
			switch state {
			case undecided, anon:
				for _, name := range names {
					result = append(result, &Variable{Type: p.typeFromWord(name.Value.(string))})
				}
				for _, typ := range types {
					result = append(result, &Variable{Type: typ})
				}
				names = nil
				break loop
			case named:
				return nil, fmt.Errorf("Last parameter needs a type")
			}
		case TOKEN_COMMA:
			p.nextToken()
			p.skipWhiteSpace()
		default:
			switch state {
			case anon:
				fmt.Errorf("Invalid arguments declaration: comma missing or a typo in argument name")
			case undecided:
				state = named
				fallthrough
			case named:
				t, err := p.parseType()
				if err != nil {
					return nil, err
				}
				for _, name := range names {
					result = append(result, &Variable{name: name.Value.(string), Type: t})
				}
				names = nil
			}

			switch p.peek().Type {
			case TOKEN_RPARENTH, TOKEN_COLON, TOKEN_INDENT:
				break loop
			case TOKEN_COMMA:
				p.nextToken()
			default:
				return nil, fmt.Errorf("Unexpected token: %s", p.peek().Type)
			}
		}
	}

	return []*VarDecl{&VarDecl{Vars: result}}, nil
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

func (p *Parser) parseGenericParams() ([]string, error) {
	t := p.expect(TOKEN_LBRACKET)
	if t == nil {
		return nil, fmt.Errorf("Expected `[`")
	}
	genericTypes := []string{}

loop:
	for {
		typeName := p.expect(TOKEN_WORD)
		if typeName == nil {
			return nil, fmt.Errorf("Expected generic type name")
		}

		name := typeName.Value.(string)

		if !p.parsingGenericInstantiation() {
			// When parsing a generic instantiation, ignore the params.
			// We're just re-parsing the code, substituting generic params occurences
			// with concrete types as we go.
			genericTypes = append(genericTypes, name)

			p.identStack.addObject(&GenericParamTypeDecl{
				stmt: stmt{expr: expr{typeName.Offset}},
				name: name},
			)
		}

		switch t := p.nextToken(); t.Type {
		case TOKEN_COMMA:
		case TOKEN_RBRACKET:
			break loop
		default:
			return nil, fmt.Errorf("Unexpected token %s", t)
		}
	}

	return genericTypes, nil
}

// Parses function header (declaration without the body).
// Returns a partially complete FuncDecl, that can be later filled with
// function's body, etc.
func (p *Parser) parseFuncHeader(genericPossible bool) (*FuncDecl, error) {
	startTok := p.expect(TOKEN_FUNC)
	if startTok == nil {
		return nil, fmt.Errorf("Function declaration needs to start with 'func' keyword")
	}

	if p.peek().Type == TOKEN_MUL {
		p.nextToken()
	}

	var err error

	funcName := ""
	genericTypes := []string{}

	t := p.nextToken()
	switch t.Type {
	case TOKEN_WORD:
		funcName = t.Value.(string)

		if p.peek().Type == TOKEN_LBRACKET {
			if !genericPossible {
				return nil, fmt.Errorf("Unexpected generic function")
			}
			genericTypes, err = p.parseGenericParams()
			if err != nil {
				return nil, err
			}
		}
	case TOKEN_LPARENTH:
		// anonymous function
		p.putBack(t)
	default:
		return nil, fmt.Errorf("Unexpected token after `func`: %s", t.Type)
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

	if p.peek().Type == TOKEN_LPARENTH {
		p.nextToken()

		results, err = p.parseArgsDecl()
		if err != nil {
			return nil, err
		}

		if p.expect(TOKEN_RPARENTH) == nil {
			return nil, fmt.Errorf("Expected `)`")
		}
	} else {
		typ, err := p.attemptTypeParse(true)
		switch err {
		case doesntLookLikeTypeErr:
			// OK, function doesn't return anything
		case nil:
			results = []*VarDecl{&VarDecl{Vars: []*Variable{&Variable{Type: typ}}}}
		default:
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
		GenericParams: genericTypes,
	}, nil
}

func (p *Parser) parseFunc(genericPossible bool) (*FuncDecl, Object, error) {
	start := p.peek()

	fd, err := p.parseFuncHeader(genericPossible)
	if err != nil {
		return nil, nil, err
	}

	var obj Object
	var gf *GenericFunc

	if len(fd.GenericParams) > 0 {
		gf = &GenericFunc{stmt: stmt{expr: fd.expr},
			params: fd.GenericParams, Func: fd, imports: p.imports}
		obj = gf
	} else {
		obj = &Variable{name: fd.name, Type: fd.typ}
	}

	if fd.Receiver == nil {
		// Add it to scope so that recursive calls can work.
		if p.parsingGenericInstantiation() {
			p.identStack.addObject(p.generic)
		} else {
			p.identStack.addObject(obj)
		}
	}

	fd, err = p.parseFuncBody(fd)

	if gf != nil {
		end := p.peek()
		code := p.lex.Slice(start, end)
		gf.code = code
	}

	return fd, obj, err
}

// Parse function body assuming that its header has been already parsed.
func (p *Parser) parseFuncBody(fd *FuncDecl) (*FuncDecl, error) {
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

	// This is used to connect return statements with functions at the time of writing.
	p.funcStack = append(p.funcStack, fd)
	defer func() { p.funcStack = p.funcStack[:len(p.funcStack)-1] }()

	block, err := p.parseCodeBlock()
	if err != nil {
		return nil, err
	}

	if p.branchTreesStack.top().CountBranchStmts() > 0 {
		return nil, fmt.Errorf("Unmatched branch statements: %#v", p.branchTreesStack.top())
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

		id = &Ident{expr{word.Offset}, word.Value.(string), nil, false}
		// TODO: lookup ident (when label parsing is implemented)
	}

	r := &BranchStmt{stmt{expr: expr{tok.Offset}}, tok, id, nil, nil}
	p.branchTreesStack.top().Members.Add(r)

	return r, nil
}

func (p *Parser) parseReturnStmt() (*ReturnStmt, error) {
	tok := p.expect(TOKEN_RETURN)
	if tok == nil {
		return nil, fmt.Errorf("Expected `return` keyword")
	}

	if len(p.funcStack) == 0 {
		return nil, fmt.Errorf("Return statement used outside a function")
	}

	s := &ReturnStmt{stmt: stmt{expr: expr{tok.Offset}}, Func: p.funcStack[len(p.funcStack)-1]}

	switch p.peek().Type {
	case TOKEN_INDENT, TOKEN_EOF:
		// This is a blank
		return s, nil
	default:
		exps, err := p.parseExprList()
		if err != nil {
			return nil, err
		}
		s.Values = exps
		return s, nil
	}
}

func (p *Parser) skipIndents() {
	for p.peek().Type == TOKEN_INDENT {
		p.nextToken()
	}
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
		p.skipIndents()
	}
	return result, nil
}

// Parse either initialization or `=` assignment. Return error for other statements.
func (p *Parser) parseInitOrAssign() (Stmt, error) {
	if p.peek().Type == TOKEN_VAR {
		return p.parseVarStmt(true)
	}

	s, err := p.parseSimpleStmt(false)
	if err != nil {
		return nil, err
	}

	assign, ok := s.(*AssignStmt)
	if !ok {
		return nil, fmt.Errorf("Expected assignment")
	}

	if assign.Token.Type != TOKEN_ASSIGN {
		return nil, fmt.Errorf("Only `=` assignment allowed")
	}

	return s, nil
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

func (p *Parser) parseStructStmt() (Stmt, error) {
	firstTok := p.peek()

	typeDecl := &TypeDecl{
		stmt: stmt{expr: expr{firstTok.Offset}},
	}

	structDecl, err := p.parseStruct(typeDecl, true)
	if err != nil {
		return nil, err
	}

	if len(structDecl.GenericParams) > 0 {
		gs := &GenericStruct{
			params:  structDecl.GenericParams,
			struc:   structDecl,
			code:    p.lex.Slice(firstTok, p.peek()),
			imports: p.imports,
		}
		p.identStack.addObject(gs)
		return gs, nil
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

func (p *Parser) parseImportStmt() (*ImportStmt, error) {
	t := p.expect(TOKEN_IMPORT)
	if t == nil {
		return nil, fmt.Errorf("Expected `import`")
	}

	t = p.expect(TOKEN_STR)
	if t == nil {
		return nil, fmt.Errorf("Expected package path")
	}

	path := t.Value.(string)
	path = path[1 : len(path)-1]
	s := strings.Split(path, "/")
	name := s[len(s)-1]

	if p.peek().Type == TOKEN_AS {
		p.nextToken()
		word := p.expect(TOKEN_WORD)
		if word == nil {
			return nil, fmt.Errorf("Expected imported package name")
		}
		name = word.Value.(string)
	}

	result := &ImportStmt{
		name: name,
		path: path,
	}

	if _, ok := p.imports[name]; ok {
		return nil, fmt.Errorf("Package named `%s` imported more than once", name)
	}

	p.imports[name] = result

	return result, nil
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
		case TOKEN_SWITCH:
			p.putBack(token)
			return p.parseSwitchStmt()
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
		case TOKEN_RETURN:
			p.putBack(token)
			return p.parseReturnStmt()
		case TOKEN_EOF:
			return nil, nil
		case TOKEN_STRUCT:
			p.putBack(token)
			return p.parseStructStmt()
		case TOKEN_INTERFACE:
			p.putBack(token)
			return p.parseIfaceStmt()
		case TOKEN_IMPORT:
			p.putBack(token)
			return p.parseImportStmt()
		default:
			p.putBack(token)
			stmt, err := p.parseSimpleStmt(true)
			p.prevLbl, _ = stmt.(*LabelStmt)
			return stmt, err
		}
	}
}

func (p *Parser) ParseFile(f *File) error {
	if p.expect(TOKEN_PACKAGE) == nil {
		return fmt.Errorf("Expected keyword `package` at the beginning of a file")
	}

	pkg := ""
	if t := p.expect(TOKEN_WORD); t == nil {
		return fmt.Errorf("Expected package name after the `package` keyword")
	} else {
		pkg = t.Value.(string)
	}

	stmts, err := p.Parse()
	if err != nil {
		return err
	}

	f.pkg, f.statements = pkg, stmts
	return nil
}

func (p *Parser) reapNewDecls() error {
	objs := (*p.identStack)[1]

	for name, obj := range objs {
		if _, ok := p.topLevelDecls[name]; ok {
			return fmt.Errorf("Redeclared `%s`", name)
		}
		p.topLevelDecls[name] = obj
	}
	p.identStack.eraseAllExceptBuiltins()
	p.identStack.pushScope()
	return nil
}

// Match top-level idents within one file. Useful in tests.
func (p *Parser) matchTopDecls(stmts []*TopLevelStmt) {
	for _, stmt := range stmts {
		types, idents := stmt.unboundTypes, stmt.unboundIdents
		for name, ts := range types {
			decl := p.topLevelDecls[name]
			if decl == nil || decl.ObjectType() != OBJECT_TYPE {
				continue
			}

			for _, t := range ts {
				t.Decl = decl.(*TypeDecl)
			}
		}

		for name, ids := range idents {
			for _, id := range ids {
				id.object = p.topLevelDecls[name]
			}
		}
	}
}

func (p *Parser) parseUnindentedBlock() ([]Stmt, error) {
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

func (p *Parser) Parse() ([]*TopLevelStmt, error) {
	var result = []*TopLevelStmt{}
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
		result = append(result, &TopLevelStmt{
			Stmt:          stmt,
			unboundTypes:  p.unboundTypes,
			unboundIdents: p.unboundIdents,
		})
		p.reapNewDecls()
		// Reset unbound types/idents before next statement
		p.unboundTypes = make(map[string][]*CustomType)
		p.unboundIdents = make(map[string][]*Ident)
	}
	return result, nil
}
