package have

import (
	"errors"
	"fmt"
	"unicode"
	"unicode/utf8"

	goscanner "go/scanner"
	gotoken "go/token"
)

type TokenType int

type Token struct {
	Type   TokenType
	Offset int
	Value  interface{}
	Pos    gotoken.Pos
}

// Tells if a token is any of the comparison operators.
func (t *Token) IsCompOp() bool {
	switch t.Type {
	case TOKEN_EQUALS, TOKEN_NEQUALS,
		TOKEN_LT, TOKEN_GT,
		TOKEN_EQ_LT, TOKEN_EQ_GT:
		return true
	}
	return false
}

// Tells if a token is any of the order operators.
func (t *Token) IsOrderOp() bool {
	switch t.Type {
	case TOKEN_LT, TOKEN_GT,
		TOKEN_EQ_LT, TOKEN_EQ_GT:
		return true
	}
	return false
}

// Tells if operator's operands can only be boolean.
func (t *Token) IsLogicalOp() bool {
	switch t.Type {
	case TOKEN_AND, TOKEN_OR:
		return true
	}
	return false
}

//go:generate stringer -type=TokenType
const (
	TOKEN_EOF          TokenType = iota + 1
	TOKEN_INDENT                 // indent - []rune of whitespace characters
	TOKEN_FOR                    // the "for" keyword
	TOKEN_WORD                   // alphanumeric word, starts witn a letter
	TOKEN_ASSIGN                 // =
	TOKEN_EQUALS                 // ==
	TOKEN_NEQUALS                // !=
	TOKEN_GT                     // >
	TOKEN_LT                     // <
	TOKEN_EQ_LT                  // <=
	TOKEN_EQ_GT                  // >=
	TOKEN_NEGATE                 // !
	TOKEN_INT                    // Integer number literal
	TOKEN_FLOAT                  // Float number literal
	TOKEN_IMAG                   // Imaginary part literal
	TOKEN_STR                    // string literal
	TOKEN_RUNE                   // rune literal
	TOKEN_DOT                    // .
	TOKEN_ELLIPSIS               // ...
	TOKEN_LPARENTH               // (
	TOKEN_RPARENTH               // )
	TOKEN_LBRACKET               // [
	TOKEN_RBRACKET               // ]
	TOKEN_LBRACE                 // {
	TOKEN_RBRACE                 // }
	TOKEN_PLUS                   // +
	TOKEN_PLUS_ASSIGN            // +=
	TOKEN_INCREMENT              // ++
	TOKEN_MINUS                  // -
	TOKEN_MINUS_ASSIGN           // -=
	TOKEN_DECREMENT              // --
	TOKEN_VAR                    // the "var" keyword
	TOKEN_IF                     // the "if" keyword
	TOKEN_ELSE                   // the "else" keyword
	TOKEN_ELIF                   // the "elif" keyword
	TOKEN_SWITCH                 // the "switch" keyword
	TOKEN_CASE                   // the "case" keyword
	TOKEN_DEFAULT                // the "default" keyword
	TOKEN_RETURN                 // the "return" keyword
	TOKEN_TRUE                   // the "true" keyword
	TOKEN_FALSE                  // the "false" keyword
	TOKEN_STRUCT                 // the "struct" keyword
	TOKEN_MAP                    // the "map" keyword
	TOKEN_FUNC                   // the "func" keyword
	TOKEN_IMPORT                 // the "import" keyword
	TOKEN_AS                     // the "as" keyword
	TOKEN_TYPE                   // the "type" keyword
	TOKEN_IN                     // the "in" keyword
	TOKEN_PASS                   // the "pass" keyword
	TOKEN_PACKAGE                // the "package" keyword
	TOKEN_BREAK                  // the "break" keyword
	TOKEN_CONTINUE               // the "continue" keyword
	TOKEN_FALLTHROUGH            // the "fallthrough" keyword
	TOKEN_GOTO                   // the "goto" keyword
	TOKEN_INTERFACE              // the "interface" keyword
	TOKEN_NIL                    // the "nil" keyword
	TOKEN_CHAN                   // the "chan" keyword
	TOKEN_RANGE                  // the "range" keyword
	TOKEN_WHEN                   // the "when" keyword
	TOKEN_IMPLEMENTS             // the "implements" keyword
	TOKEN_IS                     // the "is" keyword
	TOKEN_MUL                    // *
	TOKEN_DIV                    // /
	TOKEN_MUL_ASSIGN             // *=
	TOKEN_DIV_ASSIGN             // /=
	TOKEN_SHL                    // <<
	TOKEN_SHR                    // >>
	TOKEN_SEND                   // <-
	TOKEN_COMMA                  // ,
	TOKEN_COLON                  // :
	TOKEN_SEMICOLON              // ;
	TOKEN_AMP                    // &
	TOKEN_PIPE                   // |
	TOKEN_PERCENT                // %
	TOKEN_AND                    // &&
	TOKEN_OR                     // ||
	TOKEN_SHARP                  // #
	TOKEN_UNEXP_CHAR             // For error reporting
)

type Lexer struct {
	// All characters, immutable.
	all []rune
	// Characters not processed yet.
	buf []rune
	// Stack of opened indents.
	indentsStack []int
	// We don't want to emit indent tokens for blank lines,
	// so we need to postpone indent tokens for a while.
	tokenIndent *Token
	// How many characters we've processed.
	skipped int
	// Offset of currently processed token.
	curTokenPos int

	offset int

	tfile *gotoken.File
}

func NewLexer(buf []rune, tfile *gotoken.File, offset int) *Lexer {
	return &Lexer{all: buf, buf: buf, indentsStack: []int{}, tfile: tfile, offset: offset}
}

func countWhiteChars(buf []rune) int {
	i := 0
	for i < len(buf) && (unicode.IsSpace(buf[i]) && buf[i] != '\n') {
		i++
	}
	return i
}

// Advance lexer's buffer by skipping whitespace, except newlines.
func (l *Lexer) skipWhiteChars() []rune {
	i := countWhiteChars(l.buf)
	whitespace := l.buf[:i]
	l.skipBy(i)
	return whitespace
}

func (l *Lexer) skipLine() []rune {
	c := 0
	for c < len(l.buf) && l.buf[c] != '\n' {
		c++
	}
	line := l.buf[0:c]
	l.skipBy(c)
	return line
}

func (l *Lexer) skipInlineComment() []rune {
	if len(l.buf) >= 2 && string(l.buf[:2]) == "//" {
		l.skipBy(2)
		return l.skipLine()
	}
	return nil
}

func (l *Lexer) skipMultilineComment() ([]rune, error) {
	c := 0
	for c < len(l.buf)-1 {
		if string(l.buf[c:c+2]) == "*/" {
			comment := l.buf[0:c]
			l.skipBy(c + 2) // +2 to include the "*/"
			return comment, nil
		}
		c++
	}
	return nil, errors.New("Did not close comment")
}

// Skip whitespace and comments
func (l *Lexer) skipFluff() {
	for {
		before := len(l.buf)
		l.skipWhiteChars()
		l.skipInlineComment()
		after := len(l.buf)
		if before == after {
			break
		}
	}
}

// Read an alphanumeric word from the buffer, advancing it.
func (l *Lexer) scanWord() []rune {
	i := 0
	for i < len(l.buf) && (unicode.IsLetter(l.buf[i]) || unicode.IsNumber(l.buf[i]) || l.buf[i] == '_') {
		i++
	}

	result := l.buf[:i]
	l.skipBy(i)
	return result
}

// Advance lexer's buffer by one character.
func (l *Lexer) skip() {
	l.skipped++
	l.buf = l.buf[1:]
}

// Advance lexer's buffer by N characters.
func (l *Lexer) skipBy(n int) {
	l.skipped += n
	l.buf = l.buf[n:]
}

// Tells if we've reached the end of the buffer.
func (l *Lexer) isEnd() bool {
	return len(l.buf) == 0
}

// Check which token is currently at the beginning of the buffer.
// Can be used to decide between tokens with the same beginning, e.g.
// "=", "==", "=<", ">=".
// Returns the first token matched, NOT the longest one, so order matters.
// E.g. instead of "=", "=="; rather use "==", "=".
func (l *Lexer) checkAlt(alts ...string) (alt string, ok bool) {
	for _, alt := range alts {
		if len(l.buf) >= len(alt) && string(l.buf[:len(alt)]) == alt {
			l.skipBy(len(alt))
			return alt, true
		}
	}
	return "", false
}

func (l *Lexer) loadEscapedString() (string, error) {
	if len(l.buf) == 0 || l.buf[0] != '"' {
		return "", fmt.Errorf("String literal has to start with a double quote")
	}

	l.skip()

	i := 0
	for ; i < len(l.buf); i++ {
		switch l.buf[i] {
		case '\\':
			i++
			if i == len(l.buf) {
				return "", fmt.Errorf("Unexpected file end - middle of a string literal")
			}
		case '"':
			s := string(l.buf[:i])
			l.skipBy(i + 1)
			return s, nil
		}
	}
	return "", fmt.Errorf("Unterminated string literal")
}

func (l *Lexer) newToken(typ TokenType, val interface{}) *Token {
	return &Token{Type: typ, Offset: l.curTokenPos, Value: val, Pos: l.tfile.Pos(l.curTokenPos + l.offset)}
}

// A convenience wrapper for newToken, handy in situations when a token
// is created just to be immediately returned with a nil error.
func (l *Lexer) retNewToken(typ TokenType, val interface{}) *Token {
	return l.newToken(typ, val)
}

func (l *Lexer) scanGoToken() (token gotoken.Token, lit string, err error) {
	// TODO: We shouldn't be setting everything up from scratch every time.

	fs := gotoken.NewFileSet()
	code := make([]byte, 0, len(l.buf))
	tmp := make([]byte, 3)

	// TODO: Don't use []rune, if Golang doesn't need it neither do we and it leads
	// to stuff like this.
	for i := 0; i < len(l.buf); i++ {
		l := utf8.EncodeRune(tmp, l.buf[i])
		code = append(code, tmp[:l]...)
	}

	f := fs.AddFile("", fs.Base(), len(code))
	s := &goscanner.Scanner{}

	errorHandler := func(pos gotoken.Position, msg string) {
		err = fmt.Errorf("Scanner error: %s, %s", pos, msg)
	}

	s.Init(f, []byte(code), errorHandler, 0)
	_, tok, lit := s.Scan()
	l.skipBy(len([]rune(lit)))

	return tok, lit, err
}

func (l *Lexer) fromGoToken(token gotoken.Token, lit string) *Token {
	switch token {
	case gotoken.INT:
		return l.retNewToken(TOKEN_INT, lit)
	case gotoken.FLOAT:
		return l.retNewToken(TOKEN_FLOAT, lit)
	case gotoken.IMAG:
		return l.retNewToken(TOKEN_IMAG, lit)
	case gotoken.STRING:
		return l.retNewToken(TOKEN_STR, lit)
	case gotoken.CHAR:
		return l.retNewToken(TOKEN_RUNE, lit)
	}
	return l.newToken(TOKEN_UNEXP_CHAR, lit)
}

// Returns fragment of code [start, end)
func (l *Lexer) Slice(start, end *Token) []rune {
	return l.all[start.Offset:end.Offset]
}

func (l *Lexer) Next() *Token {
	l.curTokenPos = l.skipped

	if !l.isEnd() && l.buf[0] != '\n' {
		if l.tokenIndent != nil {
			t := l.tokenIndent
			l.tokenIndent = nil
			return t
		}
	}

	if l.isEnd() {
		return l.retNewToken(TOKEN_EOF, nil)
	}

	ch := l.buf[0]

	switch {
	case ch == '\n':
		l.tfile.AddLine(l.curTokenPos)
		l.skip()
		indent := string(l.skipWhiteChars())
		l.skipInlineComment()
		l.tokenIndent = l.newToken(TOKEN_INDENT, indent)
		return l.Next()
	case unicode.IsSpace(ch):
		l.skipWhiteChars()
		return l.Next()
	case unicode.IsLetter(ch) || ch == '_':
		word := l.scanWord()
		switch s := string(word); s {
		case "for":
			return l.retNewToken(TOKEN_FOR, nil)
		case "pass":
			return l.retNewToken(TOKEN_PASS, nil)
		case "package":
			return l.retNewToken(TOKEN_PACKAGE, nil)
		case "var":
			return l.retNewToken(TOKEN_VAR, nil)
		case "if":
			return l.retNewToken(TOKEN_IF, nil)
		case "else":
			return l.retNewToken(TOKEN_ELSE, nil)
		case "elif":
			return l.retNewToken(TOKEN_ELIF, nil)
		case "switch":
			return l.retNewToken(TOKEN_SWITCH, nil)
		case "case":
			return l.retNewToken(TOKEN_CASE, nil)
		case "default":
			return l.retNewToken(TOKEN_DEFAULT, nil)
		case "return", "ret":
			return l.retNewToken(TOKEN_RETURN, nil)
		case "true":
			return l.retNewToken(TOKEN_TRUE, nil)
		case "false":
			return l.retNewToken(TOKEN_FALSE, nil)
		case "struct":
			return l.retNewToken(TOKEN_STRUCT, nil)
		case "interface":
			return l.retNewToken(TOKEN_INTERFACE, nil)
		case "map":
			return l.retNewToken(TOKEN_MAP, nil)
		case "func":
			return l.retNewToken(TOKEN_FUNC, nil)
		case "import":
			return l.retNewToken(TOKEN_IMPORT, nil)
		case "as":
			return l.retNewToken(TOKEN_AS, nil)
		case "type":
			return l.retNewToken(TOKEN_TYPE, nil)
		case "break":
			return l.retNewToken(TOKEN_BREAK, nil)
		case "continue":
			return l.retNewToken(TOKEN_CONTINUE, nil)
		case "fallthrough":
			return l.retNewToken(TOKEN_FALLTHROUGH, nil)
		case "goto":
			return l.retNewToken(TOKEN_GOTO, nil)
		case "nil":
			return l.retNewToken(TOKEN_NIL, nil)
		case "chan":
			return l.retNewToken(TOKEN_CHAN, nil)
		case "range":
			return l.retNewToken(TOKEN_RANGE, nil)
		case "when":
			return l.retNewToken(TOKEN_WHEN, nil)
		case "implements":
			return l.retNewToken(TOKEN_IMPLEMENTS, nil)
		case "is":
			return l.retNewToken(TOKEN_IS, nil)
		default:
			return l.retNewToken(TOKEN_WORD, s)
		}
	case ch == '=':
		alt, _ := l.checkAlt("==", "=")
		switch alt {
		case "=":
			return l.retNewToken(TOKEN_ASSIGN, alt)
		case "==":
			return l.retNewToken(TOKEN_EQUALS, alt)
		}
	case ch == '!':
		alt, _ := l.checkAlt("!=", "!")
		switch alt {
		case "!=":
			return l.retNewToken(TOKEN_NEQUALS, alt)
		case "!":
			return l.retNewToken(TOKEN_NEGATE, alt)
		}
	case ch == '+':
		alt, _ := l.checkAlt("++", "+=", "+")
		switch alt {
		case "+":
			return l.retNewToken(TOKEN_PLUS, alt)
		case "+=":
			return l.retNewToken(TOKEN_PLUS_ASSIGN, alt)
		case "++":
			return l.retNewToken(TOKEN_INCREMENT, alt)
		}
	case ch == '-':
		alt, _ := l.checkAlt("--", "-=", "-")
		switch alt {
		case "-":
			return l.retNewToken(TOKEN_MINUS, alt)
		case "-=":
			return l.retNewToken(TOKEN_MINUS_ASSIGN, alt)
		case "--":
			return l.retNewToken(TOKEN_DECREMENT, alt)
		}
	case ch == '<':
		alt, _ := l.checkAlt("<<", "<-", "<=", "<")
		switch alt {
		case "<":
			return l.retNewToken(TOKEN_LT, alt)
		case "<-":
			return l.retNewToken(TOKEN_SEND, alt)
		case "<<":
			return l.retNewToken(TOKEN_SHL, alt)
		case "<=":
			return l.retNewToken(TOKEN_EQ_LT, alt)
		}
	case ch == '>':
		alt, _ := l.checkAlt(">>", ">=", ">")
		switch alt {
		case ">":
			return l.retNewToken(TOKEN_GT, alt)
		case ">>":
			return l.retNewToken(TOKEN_SHR, alt)
		case ">=":
			return l.retNewToken(TOKEN_EQ_GT, alt)
		}
	case unicode.IsNumber(ch) || ch == '"' || ch == '`' || ch == '\'':
		gotok, lit, err := l.scanGoToken()
		if err != nil {
			return nil
		}
		return l.fromGoToken(gotok, lit)
	case ch == '(':
		l.skip()
		return l.retNewToken(TOKEN_LPARENTH, nil)
	case ch == ')':
		l.skip()
		return l.retNewToken(TOKEN_RPARENTH, nil)
	case ch == '[':
		l.skip()
		return l.retNewToken(TOKEN_LBRACKET, nil)
	case ch == ']':
		l.skip()
		return l.retNewToken(TOKEN_RBRACKET, nil)
	case ch == '{':
		l.skip()
		return l.retNewToken(TOKEN_LBRACE, nil)
	case ch == '}':
		l.skip()
		return l.retNewToken(TOKEN_RBRACE, nil)
	case ch == '.':
		alt, _ := l.checkAlt("...", ".")
		switch alt {
		case "...":
			return l.retNewToken(TOKEN_ELLIPSIS, alt)
		case ".":
			return l.retNewToken(TOKEN_DOT, alt)
		}
	case ch == '*':
		alt, _ := l.checkAlt("*=", "*")
		switch alt {
		case "*":
			return l.retNewToken(TOKEN_MUL, alt)
		case "*=":
			return l.retNewToken(TOKEN_MUL_ASSIGN, alt)
		}
	case ch == '/':
		alt, _ := l.checkAlt("//", "/=", "/*", "/")
		switch alt {
		case "//":
			l.skipLine()
			return l.Next()
		case "/":
			return l.retNewToken(TOKEN_DIV, alt)
		case "/=":
			return l.retNewToken(TOKEN_DIV_ASSIGN, alt)
		case "/*":
			_, err := l.skipMultilineComment()
			if err != nil {
				return nil
			}
			return l.Next()
		}
	case ch == ',':
		l.skip()
		return l.retNewToken(TOKEN_COMMA, nil)
	case ch == ';':
		l.skip()
		return l.retNewToken(TOKEN_SEMICOLON, nil)
	case ch == ':':
		l.skip()
		return l.retNewToken(TOKEN_COLON, nil)
	case ch == '%':
		l.skip()
		return l.retNewToken(TOKEN_PERCENT, "%")
	case ch == '&':
		alt, _ := l.checkAlt("&&", "&")
		switch alt {
		case "&&":
			return l.retNewToken(TOKEN_AND, alt)
		case "&":
			return l.retNewToken(TOKEN_AMP, alt)
		}
	case ch == '|':
		alt, _ := l.checkAlt("||", "|")
		switch alt {
		case "||":
			return l.retNewToken(TOKEN_OR, alt)
		case "|":
			return l.retNewToken(TOKEN_PIPE, alt)
		}
	}
	return l.newToken(TOKEN_UNEXP_CHAR, ch)
}

// Can be used to mark a fragment of code in the file and then extract it.
type fragment struct {
	from, code []rune
}

// Never call without calling EndMark first.
// The result is trimmed only on the left side.
func (f *fragment) TrimmedString() []rune {
	return f.code[countWhiteChars(f.code):]
}

func (l *Lexer) NewMark() *fragment {
	return &fragment{
		from: l.buf,
	}
}

func (l *Lexer) EndMark(f *fragment) {
	f.code = f.from[:len(f.from)-len(l.buf)]
}
