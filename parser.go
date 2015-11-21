package have

import (
	"bytes"
	"fmt"
)
import "strconv"
import "github.com/davecgh/go-spew/spew"

type Parser struct {
	lex       *Lexer
	tokensBuf []*Token
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
	return &Parser{lex: lex}
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

type Expr interface {
	Pos() int
}

type expr struct {
	pos int
}

type Stmt interface {
	Expr
}

type VarDecl struct {
	Name string
	Type Type
	Init Expr
}

type CodeBlock struct {
	Statements []Stmt
}

// implements Stmt
type VarStmt struct {
	expr
	Vars []*VarDecl
}

// implements Stmt
type IfBranch struct {
	expr
	ScopedVarDecl *VarStmt
	Condition     Expr
	Code          *CodeBlock
}

// implements Stmt
type IfStmt struct {
	expr
	Branches []*IfBranch
}

// implements Stmt
// Statement wrapper for expressions.
type ExprStmt struct {
	Expression Expr
}

type Type interface {
	// True means no underscores beneath, no type inference needed.
	Known() bool
	String() string
}

type SimpleType struct {
	Name string
}

func (t *SimpleType) Known() bool    { return true }
func (t *SimpleType) String() string { return t.Name }

type ArrayType struct {
	Size int
	Of   Type
}

func (t *ArrayType) Known() bool    { return t.Of.Known() }
func (t *ArrayType) String() string { return fmt.Sprintf("[%d]%s", t.Size, t.Of.String()) }

type SliceType struct {
	Of Type
}

func (t *SliceType) Known() bool    { return t.Of.Known() }
func (t *SliceType) String() string { return "[]" + t.Of.String() }

type MapType struct {
	By, Of Type
}

func (t *MapType) Known() bool    { return t.By.Known() && t.Of.Known() }
func (t *MapType) String() string { return "map[" + t.By.String() + "]" + t.Of.String() }

type PointerType struct {
	To Type
}

func (t *PointerType) Known() bool    { return t.To.Known() }
func (t *PointerType) String() string { return "*" + t.To.String() }

type StructType struct {
	Members map[string]Type
}

func (t *StructType) Known() bool {
	for _, t := range t.Members {
		if !t.Known() {
			return false
		}
	}
	return true
}

func (t *StructType) String() string {
	out := &bytes.Buffer{}
	out.Write([]byte("{"))
	c := 0
	for k, v := range t.Members {
		fmt.Fprintf(out, "%s: %s", k, v.String())
		c++
		if c < len(t.Members) {
			out.Write([]byte(", "))
		}
	}
	out.Write([]byte("}"))
	return out.String()
}

type CustomType struct {
	Name string
}

func (t *CustomType) Known() bool    { return true }
func (t *CustomType) String() string { return t.Name }

type UnknownType struct {
}

func (t *UnknownType) Known() bool    { return false }
func (t *UnknownType) String() string { return "_" }

type TypeExpr struct {
	expr
	typ Type
}

func (e *expr) Pos() int {
	return e.pos
}

// Blank expression, represents no expression.
// Sometimes useful.
// implements Expr
type BlankExpr struct {
	expr
}

func NewBlankExpr() *BlankExpr { return &BlankExpr{expr{0}} }

// implements Expr
type BasicLit struct {
	expr

	token *Token
}

type CompoundLitKind int

const (
	COMPOUND_UNKNOWN CompoundLitKind = iota
	COMPOUND_EMPTY
	COMPOUND_LISTLIKE
	COMPOUND_MAPLIKE
)

// implements Expr
type CompoundLit struct {
	expr
	typ   Type
	kind  CompoundLitKind
	elems []Expr
}

// implements Expr
type BinaryOp struct {
	expr

	Left, Right Expr
	op          *Token
}

// implements Expr
type UnaryOp struct {
	expr

	Right Expr
	op    *Token
}

type PrimaryExpr interface {
	Expr
}

// implements PrimaryExpr
type ArrayExpr struct {
	expr

	Left, Index Expr
}

type DotSelector struct {
	expr

	Left  Expr
	Right *Ident
}

// implements PrimaryExpr
type FuncCall struct {
	expr

	Left Expr
	//args []*Expr
	Args []Expr
}

// implements PrimaryExpr
type Ident struct {
	expr

	name string
	//token *Token
}

type Node interface {
	//Pos() int // TODO
	//End() int // TODO
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
	// TOKEN_BR is the only whitespace token we currently have.
	// Once we have tokens for comments they probably should be
	// handled here as well.
	for ; t.Type == TOKEN_BR; t = p.nextToken() {
	}
	p.putBack(t)
}

func (p *Parser) parseCodeBlock() (*CodeBlock, error) {
	p.skipWhiteSpace()

	if ident := p.expect(TOKEN_NEWSCOPE); ident == nil {
		return nil, fmt.Errorf("Expected a nested block of code")
	}

	result := make([]Stmt, 0)

	for t := p.nextToken(); t.Type != TOKEN_ENDSCOPE && t.Type != TOKEN_EOF; t = p.nextToken() {
		p.putBack(t) // It was part of an inner statement, put it back

		stmt, err := p.parseStmt()
		if err != nil {
			return nil, err
		}

		result = append(result, stmt)
		p.skipWhiteSpace()
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
		scopedVarDecl, err = p.parseVarDecl()
		if err != nil {
			return nil, err
		}

		scolon := p.expect(TOKEN_SEMICOLON)
		if scolon == nil {
			return nil, fmt.Errorf("`;` expected")
		}
	}

	// TODO: We should rather to this:
	//condition, err := p.parseExpr()
	condition := p.parseExpr()
	if condition == nil {
		// TODO NOW: parseExpr should return err (and all functions below it too)
		return nil, fmt.Errorf("Couldn't parse the condition expression")
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

func (p *Parser) parseVarDecl() (*VarStmt, error) {
	ident := p.expect(TOKEN_VAR)
	if ident == nil {
		return nil, fmt.Errorf("Impossible happened")
	}
	unknownType := &UnknownType{}
	vars := []*VarDecl{}
	var err error

	// Parse left side of "="
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
			for i := len(vars) - 1; i >= 0; i-- {
				if vars[i].Type == unknownType {
					vars[i].Type = decl.Type
				} else {
					break
				}
			}
			token = p.nextToken()
		}

		vars = append(vars, decl)

		switch token.Type {
		case TOKEN_COMMA:
		case TOKEN_ASSIGN:
			break loop
		case TOKEN_BR, TOKEN_SEMICOLON:
			p.putBack(token)
			// All default values.
			for _, v := range vars {
				v.Init = NewBlankExpr()
			}
			return &VarStmt{expr{ident.Offset}, vars}, nil
		default:
			return nil, fmt.Errorf("Unexpected token")
		}
	}

	// Right side of "="
	if len(vars) == 0 {
		return nil, fmt.Errorf("No vars declared on the right side of \"=\"")
	}

	inits, err := p.parseArgs()
	if err != nil {
		return nil, err
	}

	if len(inits) != len(vars) {
		return nil, fmt.Errorf("Different number of new vars and initializers\n")
	}

	for i := range vars {
		vars[i].Init = inits[i]
	}
	return &VarStmt{expr{ident.Offset}, vars}, nil
}

func (p *Parser) parseCompoundLit() (*CompoundLit, error) {
	startTok := p.expect(TOKEN_LBRACE)
	if startTok == nil {
		return nil, fmt.Errorf("Compound literal has to start with `{`")
	}

	if t := p.nextToken(); t.Type == TOKEN_RBRACE {
		return &CompoundLit{typ: &UnknownType{}, kind: COMPOUND_EMPTY, elems: nil}, nil
	} else {
		p.putBack(t)
	}

	kind := COMPOUND_UNKNOWN
	elems := []Expr{}

	for i := 0; true; i++ {
		el := p.parseExpr()
		if el == nil {
			return nil, fmt.Errorf("Expected expression within a compound literal")
		}

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
	if !p.expectSeries(TOKEN_STRUCT, TOKEN_COLON, TOKEN_BR, TOKEN_NEWSCOPE) {
		return nil, fmt.Errorf("Couldn't parse struct declaration")
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
		case TOKEN_BR:
			// Struct continues.
		case TOKEN_ENDSCOPE, TOKEN_EOF:
			return result, nil
		default:
			return nil, fmt.Errorf("Expected struct member name")
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

func (p *Parser) parsePrimaryExpr() PrimaryExpr {
	token := p.nextToken()
	var left Expr
	var err error

	switch token.Type {
	case TOKEN_LPARENTH:
		left = p.parseExpr()
		if p.expect(TOKEN_RPARENTH) == nil {
			// TODO: return error
			return nil
		}
	case TOKEN_WORD:
		left = &Ident{expr: expr{token.Offset}, name: token.Value.(string)}
		//next := p.nextToken()
	case TOKEN_STR, TOKEN_NUM, TOKEN_TRUE, TOKEN_FALSE:
		return &BasicLit{expr{token.Offset}, token}
	case TOKEN_MAP, TOKEN_STRUCT, TOKEN_LBRACKET:
		p.putBack(token)
		left, err = p.parseTypeExpr()
		if err != nil {
			// TODO: report error
			return nil
		}
	case TOKEN_LBRACE:
		// Untyped compound literal, we'll have to deduce its type.
		p.putBack(token)
		left, err = p.parseCompoundLit()
		if err != nil {
			// TODO: report error
			return nil
		}
	default:
		// TODO: report error
		return nil
	}

loop:
	for {
		token = p.nextToken()
		switch token.Type {
		case TOKEN_DOT:
			// TODO: parse type assertions
			selector := p.expect(TOKEN_WORD)
			left = &DotSelector{expr{token.Offset}, left, &Ident{expr{selector.Offset}, selector.Value.(string)}}
		case TOKEN_LPARENTH:
			args, err := p.parseArgs()
			if err != nil {
				return nil // TODO: report error
			}
			left = &FuncCall{expr{token.Offset}, left, args}
		case TOKEN_LBRACKET:
			index := p.parseExpr()
			left = &ArrayExpr{expr{token.Offset}, left, index}
		case TOKEN_LBRACE:
			p.putBack(token)
			literal, err := p.parseCompoundLit()
			if err != nil {
				return nil // TODO: report error
			}

			switch t := left.(type) {
			case *BasicLit:
				literal.typ = &CustomType{Name: t.token.Value.(string)}
			case *TypeExpr:
				literal.typ = t.typ
			default:
				return nil // TODO: report error
			}
		default:
			p.putBack(token)
			break loop
		}
	}

	return left
}

// Return primary expression, possibly wrapped in an unary operator
func (p *Parser) parseMaybeUnaryExpr() Expr {
	token := p.nextToken()
	isOp, _ := opSet[token.Type] // FIXME we should create another set with just unary operators
	if isOp {
		return &UnaryOp{op: token, Right: p.parsePrimaryExpr()}
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

func (p *Parser) parseExpr() Expr {
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
		expr := p.parseMaybeUnaryExpr()
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
			return exprStack[0]
		}
	}

	// TODO NOW
	return nil
}

func (p *Parser) parseArgs() ([]Expr, error) {
	result := []Expr{}
	for {
		token := p.nextToken()
		switch token.Type {
		case TOKEN_EOF, TOKEN_RPARENTH, TOKEN_BR, TOKEN_ENDSCOPE, TOKEN_SEMICOLON:
			p.putBack(token)
			return result, nil
		case TOKEN_COMMA:
			// nada
		default:
			p.putBack(token)
			expr := p.parseExpr()
			result = append(result, expr)
		}
	}
	return result, nil
}

func (p *Parser) parseStmt() (Stmt, error) {
	token := p.nextToken()
	switch token.Type {
	case TOKEN_VAR:
		p.putBack(token)
		return p.parseVarDecl()
	case TOKEN_IF:
		p.putBack(token)
		return p.parseIf()
	}
	return nil, nil
}

func (p *Parser) Parse() Node {
	token := p.nextToken()
	switch token.Type {
	//case TOKEN_VAR:
	//	return p.parseVarDecl()
	default:
		return p.parseExpr()
	}
}
