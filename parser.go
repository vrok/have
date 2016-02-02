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

	ignoreUnknowns, dontLookup bool
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
	return &Parser{lex: lex, identStack: &IdentStack{map[string]Object{}}}
}

// Put back a token.
func (p *Parser) putBack(tok *Token) {
	if tok == nil {
		panic(fmt.Errorf("NIL tok %s", tok))
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

func (p *Parser) expectSeries(types ...TokenType) bool {
	for _, typ := range types {
		t := p.expect(typ)
		if t == nil {
			return false
		}
	}
	return true
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
		return false, fmt.Errorf("Indent expected, got %s", token)
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
// is NOT of tokenType type.
func (p *Parser) handleIndentEndOrNoToken(tokenType TokenType) (end bool, err error) {
	end, err = p.handleIndentEnd()
	if end {
		return end, err
	}
	next := p.nextToken()
	defer p.putBack(next)
	if next.Type != tokenType {
		return true, nil
	}
	return false, err
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

	if end2, err2 := p.handleIndentEnd(); end != end2 || err != err2 {
		// We know that isIndentEnd() was false, so the result of
		// handleIndentEnd() has to be the same. It is called here
		// only to update the parser state
		panic("Impossible happened")
	}

	if t := p.nextToken(); tokens[t.Type] {
		return true, t
	} else {
		p.putBack(indTok)
		p.putBack(t)
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

	result := make([]Stmt, 0)
	p.putBack(indent)

	p.identStack.pushScope()
	defer p.identStack.popScope()

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

		result = append(result, stmt)
	}

	return &CodeBlock{Statements: result}, nil
}

// Scan for ";" to see which version of statement will be parsed.
// For 'if' that can mean if there's a scoped variable declaration,
// for 'for' that can be a range/foreach loop or 3-expression one.
// We could also always initially assume scoped variable and backtrack
// on parse error, but this seems simpler.
// It restores intitial state of the parser before returning.
func (p *Parser) scanForSemicolon() bool {
	nopeStack := []*Token{}
	token := p.nextToken()
	semicolon := false
	for token.Type != TOKEN_COLON && token.Type != TOKEN_EOF {
		if token.Type == TOKEN_SEMICOLON {
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

// Expects the keyword "for" to be already consumed.
func (p *Parser) parse3ClauseForStmt() (*ForStmt, error) {
	var err error

	result := ForStmt{}

	if p.peek().Type != TOKEN_SEMICOLON {
		p.identStack.pushScope()
		defer p.identStack.popScope()

		result.ScopedVarDecls, err = p.parseVarDecl()
		if err != nil {
			return nil, err
		}

		for _, v := range result.ScopedVarDecls {
			p.identStack.addObject(v)
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
		result.RepeatExpr, err = p.parseExpr()
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

func (p *Parser) parseForStmt() (*ForStmt, error) {
	ident := p.expect(TOKEN_FOR)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}

	threeClause := p.scanForSemicolon()

	if threeClause {
		return p.parse3ClauseForStmt()
	} else {
		panic("todo")
	}
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

		scopedVarDecl, err = p.parseVarStmt()
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
			expr{ident.Offset},
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
				expr{t.Offset},
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
				expr{t.Offset},
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
		expr{ident.Offset},
		branches,
	}, nil
}

func (p *Parser) parseFuncStmt() (*VarStmt, error) {
	ident := p.expect(TOKEN_FUNC)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}
	p.putBack(ident)

	fun, err := p.parseFunc()
	if err != nil {
		return nil, err
	}
	decl := &VarDecl{name: fun.name, Type: fun.typ, Init: fun}
	// TODO: mark as final/not changeable
	p.identStack.addObject(decl)
	return &VarStmt{expr{ident.Offset}, []*VarDecl{decl}}, nil
}

func (p *Parser) parseVarStmt() (*VarStmt, error) {
	ident := p.expect(TOKEN_VAR)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}

	vars, err := p.parseVarDecl()
	if err != nil {
		return nil, err
	}

	for _, v := range vars {
		p.identStack.addObject(v)
	}

	return &VarStmt{expr{ident.Offset}, vars}, nil
}

func (p *Parser) parseVarDecl() ([]*VarDecl, error) {
	unknownType := &UnknownType{}
	allVars := []*VarDecl{}
	var err error

	// The outermost loop iterates over groups of vars that are
	// initialized separately. For example, this:
	//    var x, y int = (1, 2), z = 3
	// would be handled in two steps, one for x, y, and one for z.
groupsLoop:
	for {
		// Parse left side of "="
		vars := []*VarDecl{}
	loop:
		for {
			decl := &VarDecl{Type: unknownType}

			token := p.nextToken()
			switch token.Type {
			case TOKEN_WORD:
				decl.name = token.Value.(string)
			case TOKEN_ASSIGN:
				break loop
			default:
				return nil, fmt.Errorf("Unexpected token %s\n", token)
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
				return nil, fmt.Errorf("Unexpected token %s", token)
			}
		}

		// Right side of "="
		if len(vars) == 0 {
			return nil, fmt.Errorf("No vars declared on the left side of \"=\"")
		}

		switch t := p.nextToken(); t.Type {
		case TOKEN_COMMA:
			allVars = append(allVars, vars...)
			continue groupsLoop
		case TOKEN_ASSIGN:
			// Go on
		case TOKEN_INDENT, TOKEN_SEMICOLON, TOKEN_RPARENTH:
			p.putBack(t)
			// All default values.
			for _, v := range vars {
				v.Init = NewBlankExpr()
			}
			allVars = append(allVars, vars...)
			break groupsLoop
		default:
			return nil, fmt.Errorf("Unexpected token after new vars list: %#v", t)
		}

		var inits []Expr
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

		if len(inits) != len(vars) {
			return nil, fmt.Errorf("Different number of new vars and initializers\n")
		}

		for i := range vars {
			vars[i].Init = inits[i]
		}

		allVars = append(allVars, vars...)

		if t := p.nextToken(); t.Type != TOKEN_COMMA {
			p.putBack(t)
			break groupsLoop
		}
	}
	return allVars, nil
}

func (p *Parser) parseCompoundLit() (*CompoundLit, error) {
	startTok := p.expect(TOKEN_LBRACE)
	if startTok == nil {
		return nil, fmt.Errorf("Compound literal has to start with `{`")
	}

	p.skipWhiteSpace()

	if t := p.nextToken(); t.Type == TOKEN_RBRACE {
		return &CompoundLit{typ: &UnknownType{}, kind: COMPOUND_EMPTY, elems: nil}, nil
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
				return &CompoundLit{expr{startTok.Offset}, &UnknownType{}, kind, elems}, nil
			default:
				return nil, fmt.Errorf("Unexpected token in a compound literal")
			}
		} else {
			switch t := p.nextToken(); t.Type {
			case TOKEN_COMMA:
			case TOKEN_RBRACE:
				return &CompoundLit{expr{startTok.Offset}, &UnknownType{}, kind, elems}, nil
			default:
				return nil, fmt.Errorf("Unexpected token in a compound literal")
			}
		}
	}
	return nil, fmt.Errorf("Impossible happened")
}

func (p *Parser) parseStruct() (*StructType, error) {
	if !p.expectSeries(TOKEN_STRUCT, TOKEN_COLON) {
		return nil, fmt.Errorf("Couldn't parse struct declaration")
	}

	_, err := p.expectNewIndent()
	if err != nil {
		return nil, err
	}

	result := &StructType{Members: map[string]Type{}, Keys: []string{}}
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
			end, err := p.handleIndentEndOrNoToken(TOKEN_WORD)
			if err != nil {
				return nil, err
			}
			if end {
				return result, nil
			}
			// Struct continues.
		default:
			p.putBack(token)
			p.forceIndentEnd()
			return result, nil
		}
	}
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
		case TOKEN_NUM:
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
		return p.parseStruct()
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
		//next := p.nextToken()
	case TOKEN_STR, TOKEN_NUM, TOKEN_TRUE, TOKEN_FALSE:
		return &BasicLit{expr{token.Offset}, nil, token}, nil
	case TOKEN_MAP, TOKEN_STRUCT, TOKEN_LBRACKET:
		p.putBack(token)
		left, err = p.parseTypeExpr()
		if err != nil {
			return nil, err
		}
	case TOKEN_LBRACE:
		// Untyped compound literal, we'll have to deduce its type.
		p.putBack(token)
		left, err = p.parseCompoundLit()
		if err != nil {
			return nil, err
		}
	default:
		return nil, fmt.Errorf("Unexpected token (expected a primary expression): %s", token)
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
			if t := p.expect(TOKEN_RPARENTH); t == nil {
				return nil, fmt.Errorf("Expected `)`")
			}
			left = &FuncCallExpr{expr{token.Offset}, left, args}
		case TOKEN_LBRACKET:
			index, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			left = &ArrayExpr{expr{token.Offset}, left, index}
		case TOKEN_LBRACE:
			p.putBack(token)
			literal, err := p.parseCompoundLit()
			if err != nil {
				return nil, err
			}

			switch t := left.(type) {
			case *Ident:
				literal.typ = &CustomType{Name: t.name}
			case *TypeExpr:
				literal.typ = t.typ
			// TODO: DotSelector for types from other packages
			default:
				return nil, fmt.Errorf("Compound literal preceded with something that can't be a type: %T", t)
			}
		default:
			p.putBack(token)
			break loop
		}
	}

	return left, nil
}

// Return primary expression, possibly wrapped in an unary operator
func (p *Parser) parseMaybeUnaryExpr() (Expr, error) {
	token := p.nextToken()
	isOp, _ := opSet[token.Type] // FIXME we should create another set with just unary operators
	if isOp {
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
	{TOKEN_MUL, TOKEN_DIV, TOKEN_AMP},
	{TOKEN_PLUS, TOKEN_MINUS},
	{TOKEN_SHL, TOKEN_SHR},
	{TOKEN_LT, TOKEN_GT, TOKEN_EQ_GT, TOKEN_EQ_LT},
	{TOKEN_EQUALS}}

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

	return nil, fmt.Errorf("Error parsing expression, couldn't reduce primary/unary expression stack")
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
	return result, nil
}

func (p *Parser) parseArgsDecl() ([]*VarDecl, error) {
	// TODO: check default values are set only for parameters at the end
	t := p.nextToken()
	p.putBack(t)
	if t.Type == TOKEN_RPARENTH {
		return nil, nil
	}
	return p.parseVarDecl()
}

func (p *Parser) parseResultDecl() ([]*VarDecl, error) {
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
		result := []*VarDecl{}

		if t.Type == TOKEN_COLON {
			return result, nil
		}

		p.putBack(t)

	loop:
		for {
			typ, err := p.parseType()
			if err != nil {
				return nil, err
			}
			result = append(result, &VarDecl{
				name: "",
				Type: typ,
				Init: NewBlankExpr(),
			})

			switch t := p.nextToken(); t.Type {
			case TOKEN_COMMA:
				// Go on
			case TOKEN_COLON:
				p.putBack(t)
				break loop
			default:
				return nil, fmt.Errorf("Unexpected token %s", t)
			}
		}
		return result, nil
	}
}

func typesFromVars(vd []*VarDecl) []Type {
	result := make([]Type, len(vd))
	for i, d := range vd {
		result[i] = d.Type
	}
	return result
}

func (p *Parser) parseFunc() (*FuncDecl, error) {
	startTok := p.expect(TOKEN_FUNC)
	if startTok == nil {
		return nil, fmt.Errorf("Function declaration needs to start with 'func' keyword")
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

	p.identStack.pushScope()
	defer p.identStack.popScope()

	args, err := p.parseArgsDecl()
	if err != nil {
		return nil, err
	}

	// Make arguments accessiable within the function body.
	for _, arg := range args {
		p.identStack.addObject(arg)
	}

	if t := p.expect(TOKEN_RPARENTH); t == nil {
		return nil, fmt.Errorf("Expected `)`")
	}

	results := []*VarDecl{}

	// Check if ':' is next - if so, function doesn't return anything.
	t = p.nextToken()
	if t.Type != TOKEN_COLON {
		p.putBack(t)
		results, err = p.parseResultDecl()
		if err != nil {
			return nil, err
		}

		// Make named results accessiable within the function body.
		for _, r := range results {
			p.identStack.addObject(r)
		}

		if t := p.expect(TOKEN_COLON); t == nil {
			return nil, fmt.Errorf("Expected `:`")
		}
	}

	block, err := p.parseCodeBlock()
	if err != nil {
		return nil, err
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
		Code: block,
	}, nil
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
		expr:        expr{startTok.Offset},
		name:        name.Value.(string),
		AliasedType: realType,
	}
	p.identStack.addObject(result)
	return result, nil
}

func (p *Parser) parseStmt() (Stmt, error) {
	for {
		token := p.nextToken()
		switch token.Type {
		case TOKEN_VAR:
			p.putBack(token)
			return p.parseVarStmt()
		case TOKEN_IF:
			p.putBack(token)
			return p.parseIf()
		case TOKEN_FUNC:
			p.putBack(token)
			return p.parseFuncStmt()
		case TOKEN_TYPE:
			p.putBack(token)
			return p.parseTypeDecl()
		case TOKEN_INDENT:
			if token.Value.(string) != "" {
				return nil, fmt.Errorf("Unexpected indent")
			}
		case TOKEN_EOF:
			return nil, nil
		default:
			return nil, fmt.Errorf("Unexpected token: %s, %#v", token.Type, token)
		}
	}
}

func (p *Parser) Parse() ([]Stmt, error) {
	var result = []Stmt{}
	for t := p.nextToken(); t.Type != TOKEN_EOF; t = p.nextToken() {
		p.putBack(t)
		stmt, err := p.parseStmt()
		if err != nil {
			return nil, err
		}
		result = append(result, stmt)
	}
	return result, nil
}
