package have

import (
	"fmt"
	"reflect"
	"testing"

	gotoken "go/token"
)

func testTokens(t *testing.T, input []rune, output []*Token) {
	fs := gotoken.NewFileSet()
	l := NewLexer(input, fs.AddFile("a.go", fs.Base(), len(input)), 0)
	for _, expected := range output {
		token := l.Next()
		if !reflect.DeepEqual(token.Value, expected.Value) ||
			token.Type != expected.Type || token.Offset != expected.Offset {
			fmt.Printf("Received %v instead of %v\n", token, expected)
			t.Fail()
			return
		}
	}
}

func TestEOF(t *testing.T) {
	testTokens(t, []rune(""), []*Token{&Token{TOKEN_EOF, 0, nil, 0}})
}

func TestIndents(t *testing.T) {
	testTokens(t, []rune("\n  for"), []*Token{
		&Token{TOKEN_INDENT, 0, "  ", 0},
		&Token{TOKEN_FOR, 3, nil, 0},
		&Token{TOKEN_EOF, 6, nil, 0}})

	// Don't emit indents for blank lines
	testTokens(t, []rune("\n\n \n  for"), []*Token{
		&Token{TOKEN_INDENT, 3, "  ", 0},
		&Token{TOKEN_FOR, 6, nil, 0},
		&Token{TOKEN_EOF, 9, nil, 0}})

	s := `
		  for test
		    for
		    frog
`

	testTokens(t, []rune(s), []*Token{
		&Token{TOKEN_INDENT, 0, "		  ", 0},
		&Token{TOKEN_FOR, 5, nil, 0},
		&Token{TOKEN_WORD, 9, "test", 0},
		&Token{TOKEN_INDENT, 13, "		    ", 0},
		&Token{TOKEN_FOR, 20, nil, 0},
		&Token{TOKEN_INDENT, 23, "		    ", 0},
		&Token{TOKEN_WORD, 30, "frog", 0},
		// Lines with just whitespace don't interfere with indents,
		// no matter how many whitespace chars they have. Lexer
		// simply jumps over them (hence ENDSCOPE is generated
		// from EOF, not BR in this case).
		&Token{TOKEN_INDENT, 34, "", 0},
		&Token{TOKEN_EOF, 35, nil, 0},
	})
}

func TestEquals(t *testing.T) {
	testTokens(t, []rune("for == = <= >="), []*Token{
		&Token{TOKEN_FOR, 0, nil, 0},
		&Token{TOKEN_EQUALS, 4, "==", 0},
		&Token{TOKEN_ASSIGN, 7, "=", 0},
		&Token{TOKEN_EQ_LT, 9, "<=", 0},
		&Token{TOKEN_EQ_GT, 12, ">=", 0},
		&Token{TOKEN_EOF, 14, nil, 0}})
}

func TestNumbers(t *testing.T) {
	testTokens(t, []rune("123"), []*Token{
		&Token{TOKEN_INT, 0, "123", 0},
		&Token{TOKEN_EOF, 3, nil, 0}})
}

func TestKeywords(t *testing.T) {
	testTokens(t, []rune("var for"), []*Token{
		&Token{TOKEN_VAR, 0, nil, 0},
		&Token{TOKEN_FOR, 4, nil, 0},
		&Token{TOKEN_EOF, 7, nil, 0}})
}

func TestString(t *testing.T) {
	testTokens(t, []rune("\"123\""), []*Token{
		&Token{TOKEN_STR, 0, `"123"`, 0},
		&Token{TOKEN_EOF, 5, nil, 0}})

	testTokens(t, []rune("\"12\\\"3\" hej"), []*Token{
		&Token{TOKEN_STR, 0, "\"12\\\"3\"", 0},
		&Token{TOKEN_WORD, 8, "hej", 0},
		&Token{TOKEN_EOF, 11, nil, 0}})

	testTokens(t, []rune("`12\"3` hej"), []*Token{
		&Token{TOKEN_STR, 0, "`12\"3`", 0},
		&Token{TOKEN_WORD, 7, "hej", 0},
		&Token{TOKEN_EOF, 10, nil, 0}})
}

func TestRune(t *testing.T) {
	testTokens(t, []rune("'@'"), []*Token{
		&Token{TOKEN_RUNE, 0, "'@'", 0}})
	testTokens(t, []rune("'ą'"), []*Token{
		&Token{TOKEN_RUNE, 0, "'ą'", 0}})
}

func TestBraces(t *testing.T) {
	testTokens(t, []rune("(1)"), []*Token{
		&Token{TOKEN_LPARENTH, 0, nil, 0},
		&Token{TOKEN_INT, 1, "1", 0},
		&Token{TOKEN_RPARENTH, 2, nil, 0},
		&Token{TOKEN_EOF, 3, nil, 0}})
}

func TestPlus(t *testing.T) {
	testTokens(t, []rune("+ ++ +="), []*Token{
		&Token{TOKEN_PLUS, 0, "+", 0},
		&Token{TOKEN_INCREMENT, 2, "++", 0},
		&Token{TOKEN_PLUS_ASSIGN, 5, "+=", 0},
		&Token{TOKEN_EOF, 7, nil, 0}})
}

func TestComments(t *testing.T) {
	testTokens(t, []rune("\n#bla\n \n  for"), []*Token{
		&Token{TOKEN_INDENT, 7, "  ", 0},
		&Token{TOKEN_FOR, 10, nil, 0},
		&Token{TOKEN_EOF, 13, nil, 0}})
	testTokens(t, []rune("123#bla\nfor"), []*Token{
		&Token{TOKEN_INT, 0, "123", 0},
		&Token{TOKEN_INDENT, 7, "", 0},
		&Token{TOKEN_FOR, 8, nil, 0},
		&Token{TOKEN_EOF, 11, nil, 0}})
}

func TestFragment(t *testing.T) {
	fs := gotoken.NewFileSet()
	input := []rune("1 2 3 4")
	l := NewLexer(input, fs.AddFile("a.go", fs.Base(), len(input)), 0)

	l.Next()

	m := l.NewMark()

	l.Next()
	l.Next()

	l.EndMark(m)

	if substr := string(m.TrimmedString()); substr != "2 3" {
		t.Fatalf("Not equal: '%s'", substr)
	}
}
