package have

import (
	"fmt"
	"unicode"
)

type TokenType int

type Token struct {
	Type  TokenType
	Value interface{}
}

const (
	TOKEN_EOF         TokenType = iota + 1
	TOKEN_NEWSCOPE              // "{" in other languages
	TOKEN_ENDSCOPE              // "}" in other languages
	TOKEN_FOR                   // the "for" keyword
	TOKEN_WORD                  // alphanumeric word, starts witn a letter
	TOKEN_BR                    // new line
	TOKEN_ASSIGN                // =
	TOKEN_EQUALS                // ==
	TOKEN_EQ_LT                 // =<
	TOKEN_EQ_GT                 // =>
	TOKEN_NUM                   // general token for all number literals
	TOKEN_STR                   // string literal
	TOKEN_DOT                   // .
	TOKEN_LPARENTH              // (
	TOKEN_RPARENTH              // )
	TOKEN_LBRACKET              // [
	TOKEN_RBRACKET              // ]
	TOKEN_PLUS                  // +
	TOKEN_PLUS_ASSIGN           // +=
	TOKEN_INCREMENT             // ++
	TOKEN_VAR                   // the "var" keyword
	TOKEN_GT                    // >
	TOKEN_LT                    // <
	TOKEN_MUL                   // *
	TOKEN_DIV                   // /
	TOKEN_ADD                   // +
	TOKEN_SUB                   // -
	TOKEN_SHL                   // <<
	TOKEN_SHR                   // >>
	TOKEN_SEND                  // <-
	TOKEN_SET_ADD               // +=
	TOKEN_SET_SUB               // -=
)

type Lexer struct {
	buf          []rune
	indentsStack []int
	queue        []*Token
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
	l.buf = l.buf[i:]
	return result
}

// Advance lexer's buffer by one character.
func (l *Lexer) skip() {
	l.buf = l.buf[1:]
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
			l.buf = l.buf[len(alt):]
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
			l.buf = l.buf[i+1:]
			return s, nil
		}
	}
	return "", fmt.Errorf("Unterminated string literal")
}

func (l *Lexer) Next() (*Token, error) {
	if len(l.queue) > 0 {
		return l.popFromQueue(), nil
	}

	if l.isEnd() {
		for i := 0; i < len(l.indentsStack); i++ {
			l.queue = append(l.queue, &Token{TOKEN_ENDSCOPE, nil})
		}
		l.indentsStack = l.indentsStack[:0]

		if len(l.queue) == 0 {
			return &Token{TOKEN_EOF, nil}, nil
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
			l.queue = append(l.queue, &Token{TOKEN_BR, nil})
			return l.Next()
		}

		if l.buf[0] == '\n' {
			// Whole line was just whitespace, ignore it.
			return l.Next()
		}

		l.queue = append(l.queue, &Token{TOKEN_BR, nil})

		if len(l.indentsStack) == 0 || indent > l.indentsStack[len(l.indentsStack)-1] {
			l.indentsStack = append(l.indentsStack, indent)
			l.queue = append(l.queue, &Token{TOKEN_NEWSCOPE, nil})
			return l.Next()
		} else {
			for len(l.indentsStack) > 0 && l.indentsStack[len(l.indentsStack)-1] > indent {
				l.queue = append(l.queue, &Token{TOKEN_ENDSCOPE, nil})
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
			return &Token{TOKEN_FOR, nil}, nil
		case "var":
			return &Token{TOKEN_VAR, nil}, nil
		default:
			return &Token{TOKEN_WORD, s}, nil
		}
	case ch == '=':
		alt, _ := l.checkAlt("==", "=<", "=>", "=")
		switch alt {
		case "=":
			return &Token{TOKEN_ASSIGN, nil}, nil
		case "==":
			return &Token{TOKEN_EQUALS, nil}, nil
		case "=<":
			return &Token{TOKEN_EQ_LT, nil}, nil
		case "=>":
			return &Token{TOKEN_EQ_GT, nil}, nil
		}
	case ch == '+':
		alt, _ := l.checkAlt("++", "+=", "+")
		switch alt {
		case "+":
			return &Token{TOKEN_PLUS, nil}, nil
		case "+=":
			return &Token{TOKEN_PLUS_ASSIGN, nil}, nil
		case "++":
			return &Token{TOKEN_INCREMENT, nil}, nil
		}
	case ch == '<':
		alt, _ := l.checkAlt("<<", "<-", "<")
		switch alt {
		case "<":
			return &Token{TOKEN_LT, nil}, nil
		case "<-":
			return &Token{TOKEN_SEND, nil}, nil
		case "<<":
			return &Token{TOKEN_SHL, nil}, nil
		}
	case ch == '>':
		alt, _ := l.checkAlt(">>", ">")
		switch alt {
		case ">":
			return &Token{TOKEN_GT, nil}, nil
		case ">>":
			return &Token{TOKEN_SHR, nil}, nil
		}
	case ch == '+':
		alt, _ := l.checkAlt("+=", "+")
		switch alt {
		case "+":
			return &Token{TOKEN_ADD, nil}, nil
		case "+=":
			return &Token{TOKEN_SET_ADD, nil}, nil
		}
	case ch == '-':
		alt, _ := l.checkAlt("-=", "-")
		switch alt {
		case "-":
			return &Token{TOKEN_SUB, nil}, nil
		case "-=":
			return &Token{TOKEN_SET_SUB, nil}, nil
		}
	case unicode.IsNumber(ch):
		word := l.scanWord()
		return &Token{TOKEN_NUM, string(word)}, nil
	case ch == '"':
		str, err := l.loadEscapedString()
		if err != nil {
			return nil, err
		}
		return &Token{TOKEN_STR, str}, nil
	case ch == '(':
		l.skip()
		return &Token{TOKEN_LPARENTH, nil}, nil
	case ch == ')':
		l.skip()
		return &Token{TOKEN_RPARENTH, nil}, nil
	case ch == '[':
		l.skip()
		return &Token{TOKEN_LBRACKET, nil}, nil
	case ch == ']':
		l.skip()
		return &Token{TOKEN_RBRACKET, nil}, nil
	case ch == '.':
		l.skip()
		return &Token{TOKEN_DOT, nil}, nil
	case ch == '.':
		l.skip()
		return &Token{TOKEN_DOT, nil}, nil
	}

	return nil, fmt.Errorf("Don't know what to do")
}
