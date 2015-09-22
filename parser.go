package have

import "fmt"

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

func (p *Parser) putBack(tok *Token) {
	p.tokensBuf = append([]*Token{tok}, p.tokensBuf...)
}

type Expr interface {
}

// implements Expr
type BasicLit struct {
	token *Token
}

// implements Expr
type BinaryOp struct {
	left, right Expr
	op          *Token
}

// implements Expr
type UnaryOp struct {
	right Expr
	op    *Token
}

type PrimaryExpr interface {
	Expr
}

// implements PrimaryExpr
type ArrayExpr struct {
	left, index Expr
}

type DotSelector struct {
	left  Expr
	right *Ident
}

// implements PrimaryExpr
type FuncCall struct {
	left Expr
	//args []*Expr
	args Expr
}

// implements PrimaryExpr
type Ident struct {
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

func (p *Parser) parseVarDecl() Node {
	//ident := p.expect(TOKEN_WORD)

	return nil
}

func (p *Parser) parsePrimaryExpr() PrimaryExpr {
	token := p.nextToken()
	var left Expr

	switch token.Type {
	case TOKEN_LPARENTH:
		left = p.parseExpr()
	case TOKEN_WORD:
		left = &Ident{name: token.Value.(string)}
		//next := p.nextToken()
	case TOKEN_STR, TOKEN_NUM:
		return &BasicLit{token}
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
			left = &DotSelector{left, &Ident{selector.Value.(string)}}
		case TOKEN_LPARENTH:
			args := p.parseArgs()
			left = &FuncCall{left, args}
		case TOKEN_LBRACKET:
			index := p.parseExpr()
			left = &ArrayExpr{left, index}
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
		return &UnaryOp{op: token, right: p.parsePrimaryExpr()}
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
			fmt.Printf("%#v ", op)
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

func (p *Parser) parseExpr() Node {
	exprStack := []Expr{}
	opStack := []*Token{}

	reduce := func() {
		reduced := &BinaryOp{
			left:  exprStack[len(exprStack)-2],
			right: exprStack[len(exprStack)-1],
			op:    opStack[len(opStack)-1]}
		exprStack = append(exprStack[:len(exprStack)-2], reduced)
		opStack = opStack[:len(opStack)-1]
	}

	for {
		expr := p.parseMaybeUnaryExpr()
		exprStack = append(exprStack, expr)

		op := p.nextToken()
		isOp, _ := opSet[op.Type]
		fmt.Printf("[DEBUG] for op %#v got %#v\n", op, isOp)
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

func (p *Parser) parseArgs() Node {
	return nil
}

func (p *Parser) Parse() Node {
	token := p.nextToken()
	switch token.Type {
	case TOKEN_VAR:
		return p.parseVarDecl()
	default:
		return p.parseExpr()
	}
}
