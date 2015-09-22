package have

import (
	"fmt"
	"reflect"
	"testing"
)

func testPrimaryExpr(t *testing.T, code string, expected Expr) {
	in := []rune(code)
	parser := NewParser(NewLexer(in))
	result := parser.parsePrimaryExpr()
	if !reflect.DeepEqual(result, expected) {
		fmt.Printf("Error, expected:\n   %#v\nbut got:\n   %#v\n", expected, result)
		t.Fail()
	}
}

func TestPrimaryExpr(t *testing.T) {
	testPrimaryExpr(t, "test", &Ident{"test"})
	testPrimaryExpr(t, "test(arg)", &FuncCall{left: &Ident{"test"}, args: nil}) // TODO: fill args
	testPrimaryExpr(t, "test.tere[123]",
		&ArrayExpr{
			left: &DotSelector{
				left:  &Ident{"test"},
				right: &Ident{"tere"}},
			index: &BasicLit{&Token{TOKEN_NUM, 123}}})
}

func testExpr(t *testing.T, code string, expected Expr) {
	in := []rune(code)
	parser := NewParser(NewLexer(in))
	result := parser.parseExpr()
	if !reflect.DeepEqual(result, expected) {
		fmt.Printf("Error, expected:\n   %#v\nbut got:\n   %#v\n", expected, result)
		t.Fail()
	}
}

func TestParseExpr(t *testing.T) {
	testExpr(t, "test+test", nil)
	testExpr(t, "1*2+3*4", nil)
}
