package have

import (
	"fmt"
	"unicode"
)

type Token struct {
	Type  string
	Value interface{}
}

var (
	TOKEN_EOF      string = "eof"
	TOKEN_NEWSCOPE        = "newscope"
	TOKEN_ENDSCOPE        = "endscope"
	TOKEN_FOR             = "for"
	TOKEN_WORD            = "word"
	TOKEN_BR              = "br"
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
	for i < len(l.buf) && (unicode.IsLetter(l.buf[i]) || unicode.IsLetter(l.buf[i])) {
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
		default:
			return &Token{TOKEN_WORD, s}, nil
		}

	}

	return nil, fmt.Errorf("Don't know what to do")
}
