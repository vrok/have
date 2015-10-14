package have

import (
	"fmt"
	"unicode"
)

type TokenType int

type Token struct {
	Type   TokenType
	Offset int
	Value  interface{}
}

const (
	TOKEN_EOF          TokenType = iota + 1
	TOKEN_NEWSCOPE               // "{" in other languages
	TOKEN_ENDSCOPE               // "}" in other languages
	TOKEN_FOR                    // the "for" keyword
	TOKEN_WORD                   // alphanumeric word, starts witn a letter
	TOKEN_BR                     // new line
	TOKEN_ASSIGN                 // =
	TOKEN_EQUALS                 // ==
	TOKEN_EQ_LT                  // =<
	TOKEN_EQ_GT                  // =>
	TOKEN_NUM                    // general token for all number literals
	TOKEN_STR                    // string literal
	TOKEN_DOT                    // .
	TOKEN_LPARENTH               // (
	TOKEN_RPARENTH               // )
	TOKEN_LBRACKET               // [
	TOKEN_RBRACKET               // ]
	TOKEN_PLUS                   // +
	TOKEN_PLUS_ASSIGN            // +=
	TOKEN_INCREMENT              // ++
	TOKEN_MINUS                  // -
	TOKEN_MINUS_ASSIGN           // -=
	TOKEN_DECREMENT              // --
	TOKEN_VAR                    // the "var" keyword
	TOKEN_IF                     // the "var" keyword
	TOKEN_SWITCH                 // the "switch" keyword
	TOKEN_CASE                   // the "case" keyword
	TOKEN_RETURN                 // the "return" keyword
	TOKEN_GT                     // >
	TOKEN_LT                     // <
	TOKEN_MUL                    // *
	TOKEN_DIV                    // /
	TOKEN_SHL                    // <<
	TOKEN_SHR                    // >>
	TOKEN_SEND                   // <-
	TOKEN_COMMA                  // ,
	TOKEN_SEMICOLON              // ;
)

type Lexer struct {
	// Characters not processed yet.
	buf []rune
	// Stack of opened indents.
	indentsStack []int
	// When a single char occurence produces more than one token,
	// they should be added to this queue.
	queue []*Token
	// How many characters we've processed.
	skipped int
	// Offset of currently processed token.
	curTokenPos int
}

func NewLexer(buf []rune) *Lexer {
	return &Lexer{buf: buf, indentsStack: []int{}, queue: []*Token{}}
}

// Advance lexer's buffer by skipping whitespace, except newlines.
func (l *Lexer) skipWhiteChars() int {
	counter := 0
	for len(l.buf) > 0 && (unicode.IsSpace(l.buf[0]) && l.buf[0] != '\n') {
		l.skip()
		counter++
	}
	return counter
}

// Read an alphanumeric word from the buffer, advancing it.
func (l *Lexer) scanWord() []rune {
	i := 0
	for i < len(l.buf) && (unicode.IsLetter(l.buf[i]) || unicode.IsNumber(l.buf[i])) {
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

// Token queue is used in situations when a single buffer location can emit
// many tokens. They are placed in the queue, Next() looks into it first,
// before advancing the buffer.
func (l *Lexer) popFromQueue() *Token {
	result := l.queue[0]
	l.queue = l.queue[1:]
	return result
}

// Check which token is currently at the beginning of the buffer.
// Can be used to decide between tokens with the same beginning, e.g.
// "=", "==", "=<", ">=".
// Returns the first token matched, NOT the longest one, so order matters.
// E.g. instead of "=", "=="; rather use "==", "=".
func (l *Lexer) checkAlt(alts ...string) (alt string, ok bool) {
	for _, alt := range alts {
		if string(l.buf[:len(alt)]) == alt {
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
	return &Token{Type: typ, Offset: l.curTokenPos, Value: val}
}

// A convenience wrapper for newToken, handy in situations when a token
// is created just to be immediately returned with a nil error.
func (l *Lexer) retNewToken(typ TokenType, val interface{}) (*Token, error) {
	return l.newToken(typ, val), nil
}

func (l *Lexer) Next() (*Token, error) {
	if len(l.queue) > 0 {
		return l.popFromQueue(), nil
	}

	l.curTokenPos = l.skipped

	if l.isEnd() {
		for i := 0; i < len(l.indentsStack); i++ {
			l.queue = append(l.queue, l.newToken(TOKEN_ENDSCOPE, nil))
		}
		l.indentsStack = l.indentsStack[:0]

		if len(l.queue) == 0 {
			return l.retNewToken(TOKEN_EOF, nil)
		} else {
			return l.Next()
		}
	}

	ch := l.buf[0]

	switch {
	case ch == '\n':
		l.skip()

		indent := l.skipWhiteChars()

		if l.isEnd() {
			l.queue = append(l.queue, l.newToken(TOKEN_BR, nil))
			return l.Next()
		}

		if l.buf[0] == '\n' {
			// Whole line was just whitespace, ignore it.
			return l.Next()
		}

		l.queue = append(l.queue, l.newToken(TOKEN_BR, nil))

		if len(l.indentsStack) == 0 || indent > l.indentsStack[len(l.indentsStack)-1] {
			l.indentsStack = append(l.indentsStack, indent)
			l.queue = append(l.queue, l.newToken(TOKEN_NEWSCOPE, nil))
			return l.Next()
		} else {
			for len(l.indentsStack) > 0 && l.indentsStack[len(l.indentsStack)-1] > indent {
				l.queue = append(l.queue, l.newToken(TOKEN_ENDSCOPE, nil))
				l.indentsStack = l.indentsStack[:len(l.indentsStack)-1]
			}

			if len(l.indentsStack) > 0 && l.indentsStack[len(l.indentsStack)-1] != indent {
				// Wrong indent.
				return nil, fmt.Errorf("Bad indent")
			}

			// We should finally be pointing to something meaningful in a line.
			return l.Next()
		}
	case unicode.IsSpace(ch):
		l.skipWhiteChars()
		return l.Next()
	case unicode.IsLetter(ch):
		word := l.scanWord()
		switch s := string(word); s {
		case "for":
			return l.retNewToken(TOKEN_FOR, nil)
		case "var":
			return l.retNewToken(TOKEN_VAR, nil)
		case "if":
			return l.retNewToken(TOKEN_IF, nil)
		case "switch":
			return l.retNewToken(TOKEN_SWITCH, nil)
		case "case":
			return l.retNewToken(TOKEN_CASE, nil)
		case "return", "ret":
			return l.retNewToken(TOKEN_RETURN, nil)
		default:
			return l.retNewToken(TOKEN_WORD, s)
		}
	case ch == '=':
		alt, _ := l.checkAlt("==", "=<", "=>", "=")
		switch alt {
		case "=":
			return l.retNewToken(TOKEN_ASSIGN, nil)
		case "==":
			return l.retNewToken(TOKEN_EQUALS, nil)
		case "=<":
			return l.retNewToken(TOKEN_EQ_LT, nil)
		case "=>":
			return l.retNewToken(TOKEN_EQ_GT, nil)
		}
	case ch == '+':
		alt, _ := l.checkAlt("++", "+=", "+")
		switch alt {
		case "+":
			return l.retNewToken(TOKEN_PLUS, nil)
		case "+=":
			return l.retNewToken(TOKEN_PLUS_ASSIGN, nil)
		case "++":
			return l.retNewToken(TOKEN_INCREMENT, nil)
		}
	case ch == '-':
		alt, _ := l.checkAlt("--", "-=", "-")
		switch alt {
		case "-":
			return l.retNewToken(TOKEN_MINUS, nil)
		case "-=":
			return l.retNewToken(TOKEN_MINUS_ASSIGN, nil)
		case "--":
			return l.retNewToken(TOKEN_DECREMENT, nil)
		}
	case ch == '<':
		alt, _ := l.checkAlt("<<", "<-", "<")
		switch alt {
		case "<":
			return l.retNewToken(TOKEN_LT, nil)
		case "<-":
			return l.retNewToken(TOKEN_SEND, nil)
		case "<<":
			return l.retNewToken(TOKEN_SHL, nil)
		}
	case ch == '>':
		alt, _ := l.checkAlt(">>", ">")
		switch alt {
		case ">":
			return l.retNewToken(TOKEN_GT, nil)
		case ">>":
			return l.retNewToken(TOKEN_SHR, nil)
		}
	case unicode.IsNumber(ch):
		word := l.scanWord()
		return l.retNewToken(TOKEN_NUM, string(word))
	case ch == '"':
		str, err := l.loadEscapedString()
		if err != nil {
			return nil, err
		}
		return l.retNewToken(TOKEN_STR, str)
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
	case ch == '.':
		l.skip()
		return l.retNewToken(TOKEN_DOT, nil)
	case ch == '.':
		l.skip()
		return l.retNewToken(TOKEN_DOT, nil)
	case ch == '*': // TODO: use checkAlt, "*=", etc
		l.skip()
		return l.retNewToken(TOKEN_MUL, nil)
	case ch == ',':
		l.skip()
		return l.retNewToken(TOKEN_COMMA, nil)
	case ch == ';':
		l.skip()
		return l.retNewToken(TOKEN_SEMICOLON, nil)
	}

	return nil, fmt.Errorf("Don't know what to do, '%c'", ch)
}
