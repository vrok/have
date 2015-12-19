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

	ignoreUnknowns bool
}

type IdentStack []map[string]*VarDecl

func (is *IdentStack) pushScope() {
	*is = append(*is, map[string]*VarDecl{})
}

func (is *IdentStack) popScope() {
	*is = (*is)[:len(*is)-1]
}

func (is *IdentStack) empty() bool {
	return len(*is) == 0
}

func (is *IdentStack) addVar(v *VarDecl) {
	(*is)[len(*is)-1][v.Name] = v
}

func (is *IdentStack) findVar(name string) *VarDecl {
	for i := len(*is) - 1; i >= 0; i-- {
		if v, ok := (*is)[i][name]; ok {
			return v
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

func NewParser(lex *Lexer) *Parser {
	return &Parser{lex: lex, identStack: &IdentStack{map[string]*VarDecl{}}}
}

// Put back a token.
func (p *Parser) putBack(tok *Token) {
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

// Use it in a place where you expect another line of an indented
// block of code.
// If `end` is true then this indented block ends here, and parser
// will be pointed to the beginning of the next line.
// `err` not being nil indicates some indent mismatch.
func (p *Parser) checkIndentEnd() (end bool, err error) {
	token := p.expect(TOKEN_INDENT)
	if token == nil {
		p.putBack(token)
		return false, fmt.Errorf("Indent expected")
	}
	curIdent := p.indentStack[len(p.indentStack)-1]
	ident := token.Value.(string)
	if curIdent != ident {
		if len(ident) >= len(curIdent) {
			return false, fmt.Errorf("Unexpected indent")
		}

		p.indentStack = p.indentStack[:len(p.indentStack)-1]
		p.putBack(token)
		return true, nil
	}
	return false, nil
}

// This is very similar to checkIndentEnd, but the indented block
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
func (p *Parser) checkIndentEndOrToken(tokenType TokenType) (end bool, err error) {
	end, err = p.checkIndentEnd()
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

// Very similar to checkIndentEndOrToken, but checks if the next token
// is NOT of tokenType type.
func (p *Parser) checkIndentEndOrNoToken(tokenType TokenType) (end bool, err error) {
	end, err = p.checkIndentEnd()
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

func (p *Parser) forceIndentEnd() {
	p.indentStack = p.indentStack[:len(p.indentStack)-1]
}

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
		p.putBack(t) // So that we can use checkIndentEnd
		end, err := p.checkIndentEnd()
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

func (p *Parser) parseIf() (*IfStmt, error) {
	ident := p.expect(TOKEN_IF)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}

	// Scan for ";" to see if there's a scoped variable declaration.
	// We could also always initially assume scoped variable and backtrack
	// on parse error, but this seems simpler.
	nopeStack := []*Token{}
	token := p.nextToken()
	scopedVar := false
	for token.Type != TOKEN_COLON && token.Type != TOKEN_EOF {
		if token.Type == TOKEN_SEMICOLON {
			scopedVar = true
			break
		}
		nopeStack = append(nopeStack, token)
		token = p.nextToken()
	}
	nopeStack = append(nopeStack, token)

	p.putBackStack(nopeStack)

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

	condition, err := p.parseExpr()
	if err != nil {
		return nil, fmt.Errorf("Couldn't parse the condition expression: %s", err)
	}

	colon := p.expect(TOKEN_COLON)
	if colon == nil {
		return nil, fmt.Errorf("Expected `:` at the end of `if` condition")
	}

	block, err := p.parseCodeBlock()
	if err != nil {
		return nil, err
	}

	// TODO: else, elsif statements

	return &IfStmt{
		expr{ident.Offset},
		[]*IfBranch{&IfBranch{
			expr{ident.Offset},
			scopedVarDecl,
			condition,
			block,
		}},
	}, nil
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
		p.identStack.addVar(v)
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
				decl.Name = token.Value.(string)
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
			inits, err = p.parseArgs()
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
					restInits, err := p.parseArgs()
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
			inits, err = p.parseArgs()
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

		el, err := p.parseExpr()
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

	result := &StructType{Members: map[string]Type{}}
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
		case TOKEN_INDENT:
			p.putBack(token)
			end, err := p.checkIndentEndOrNoToken(TOKEN_WORD)
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
		switch name {
		case "bool", "byte", "complex128", "complex64", "error", "float32",
			"float64", "int", "int16", "int32", "int64", "int8", "rune",
			"string", "uint", "uint16", "uint32", "uint64", "uint8", "uintptr":
			return &SimpleType{Name: name}, nil
		default:
			return &CustomType{Name: name}, nil
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
		if v := p.identStack.findVar(name); v == nil && !p.ignoreUnknowns {
			return nil, fmt.Errorf("Unknown identifier: %s", name)
		}
		left = &Ident{expr: expr{token.Offset}, name: name}
		//next := p.nextToken()
	case TOKEN_STR, TOKEN_NUM, TOKEN_TRUE, TOKEN_FALSE:
		return &BasicLit{expr{token.Offset}, token}, nil
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
		return nil, fmt.Errorf("Unexpected token (expected a primary expression): %c", token)
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
			args, err := p.parseArgs()
			if err != nil {
				return nil, err
			}
			left = &FuncCall{expr{token.Offset}, left, args}
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
		primaryExpr, err := p.parsePrimaryExpr()
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
	{TOKEN_MUL, TOKEN_DIV},
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
	fmt.Printf("\n")
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

func (p *Parser) parseArgs() ([]Expr, error) {
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
				Name: "",
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

func (p *Parser) parseFunc() (Expr, error) {
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
		p.identStack.addVar(arg)
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
			p.identStack.addVar(r)
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
		Name:    funcName,
		Args:    args,
		Results: results,
		Code:    block,
	}, nil
}

func (p *Parser) parseStmt() (Stmt, error) {
	token := p.nextToken()
	switch token.Type {
	case TOKEN_VAR:
		p.putBack(token)
		return p.parseVarStmt()
	case TOKEN_IF:
		p.putBack(token)
		return p.parseIf()
	}
	return nil, nil
}

func (p *Parser) Parse() (Node, error) {
	token := p.nextToken()
	switch token.Type {
	//case TOKEN_VAR:
	//	return p.parseVarDecl()
	default:
		return p.parseExpr()
	}
}
