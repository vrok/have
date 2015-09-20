package have

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
	left, right *Expr
	op          *Token
}

// implements Expr
type UnaryOp struct {
	right *Expr
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

func (p *Parser) parseExpr() Node {
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
