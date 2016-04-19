package have

import (
	"fmt"
	"reflect"
	"testing"
)

func testTokens(t *testing.T, input []rune, output []*Token) {
	l := NewLexer(input)
	for _, expected := range output {
		token, err := l.Next()
		if err != nil {
			fmt.Printf("Non-nil error %v\n", err)
			t.Fail()
		}
		if !reflect.DeepEqual(token, expected) {
			fmt.Printf("Received %v instead of %v\n", token, expected)
			t.Fail()
			return
		}
	}
}

func TestEOF(t *testing.T) {
	testTokens(t, []rune(""), []*Token{&Token{TOKEN_EOF, 0, nil}})
}

func TestIndents(t *testing.T) {
	testTokens(t, []rune("\n  for"), []*Token{
		&Token{TOKEN_INDENT, 0, "  "},
		&Token{TOKEN_FOR, 3, nil},
		&Token{TOKEN_EOF, 6, nil}})

	// Don't emit indents for blank lines
	testTokens(t, []rune("\n\n \n  for"), []*Token{
		&Token{TOKEN_INDENT, 3, "  "},
		&Token{TOKEN_FOR, 6, nil},
		&Token{TOKEN_EOF, 9, nil}})

	s := `
		  for test
		    for
		    frog
`

	testTokens(t, []rune(s), []*Token{
		&Token{TOKEN_INDENT, 0, "		  "},
		&Token{TOKEN_FOR, 5, nil},
		&Token{TOKEN_WORD, 9, "test"},
		&Token{TOKEN_INDENT, 13, "		    "},
		&Token{TOKEN_FOR, 20, nil},
		&Token{TOKEN_INDENT, 23, "		    "},
		&Token{TOKEN_WORD, 30, "frog"},
		// Lines with just whitespace don't interfere with indents,
		// no matter how many whitespace chars they have. Lexer
		// simply jumps over them (hence ENDSCOPE is generated
		// from EOF, not BR in this case).
		&Token{TOKEN_INDENT, 34, ""},
		&Token{TOKEN_EOF, 35, nil},
	})
}

func TestEquals(t *testing.T) {
	testTokens(t, []rune("for == = <= >="), []*Token{
		&Token{TOKEN_FOR, 0, nil},
		&Token{TOKEN_EQUALS, 4, "=="},
		&Token{TOKEN_ASSIGN, 7, "="},
		&Token{TOKEN_EQ_LT, 9, "<="},
		&Token{TOKEN_EQ_GT, 12, ">="},
		&Token{TOKEN_EOF, 14, nil}})
}

func TestNumbers(t *testing.T) {
	testTokens(t, []rune("123"), []*Token{
		&Token{TOKEN_INT, 0, "123"},
		&Token{TOKEN_EOF, 3, nil}})
}

func TestKeywords(t *testing.T) {
	testTokens(t, []rune("var for"), []*Token{
		&Token{TOKEN_VAR, 0, nil},
		&Token{TOKEN_FOR, 4, nil},
		&Token{TOKEN_EOF, 7, nil}})
}

func TestString(t *testing.T) {
	testTokens(t, []rune("\"123\""), []*Token{
		&Token{TOKEN_STR, 0, "123"},
		&Token{TOKEN_EOF, 5, nil}})

	testTokens(t, []rune("\"12\\\"3\" hej"), []*Token{
		&Token{TOKEN_STR, 0, "12\\\"3"},
		&Token{TOKEN_WORD, 8, "hej"},
		&Token{TOKEN_EOF, 11, nil}})
}

func TestBraces(t *testing.T) {
	testTokens(t, []rune("(1)"), []*Token{
		&Token{TOKEN_LPARENTH, 0, nil},
		&Token{TOKEN_INT, 1, "1"},
		&Token{TOKEN_RPARENTH, 2, nil},
		&Token{TOKEN_EOF, 3, nil}})
}

func TestPlus(t *testing.T) {
	testTokens(t, []rune("+ ++ +="), []*Token{
		&Token{TOKEN_PLUS, 0, "+"},
		&Token{TOKEN_INCREMENT, 2, "++"},
		&Token{TOKEN_PLUS_ASSIGN, 5, "+="},
		&Token{TOKEN_EOF, 7, nil}})
}
