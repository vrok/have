package have

import (
	"fmt"
	"reflect"
	"testing"
)

func compareExpr(a, b Expr) (equal bool, msg string) {
	if reflect.TypeOf(a).Name() != reflect.TypeOf(b).Name() {
		return false, fmt.Sprintf("Types don't match for %#v and %#v", a, b)
	}
	if a.Pos() != b.Pos() {
		return false, fmt.Sprintf("Expressions have different offsets, %d for %#v and %d for %#v", a.Pos(), a, b.Pos(), b)
	}
	valA, valB := reflect.Indirect(reflect.ValueOf(a)), reflect.Indirect(reflect.ValueOf(b))
	l := valA.NumField()
	typeOfExpr := reflect.TypeOf((*Expr)(nil)).Elem()
	for i := 0; i < l; i++ {
		field := valA.Field(i)
		if field.Type().Implements(typeOfExpr) {
			eq, msg := compareExpr(
				field.
					Interface().(Expr),
				valB.Field(i).Interface().(Expr))
			if !eq {
				return false, msg
			}
		}
	}
	return true, ""
}

func testPrimaryExpr(t *testing.T, code string, expected Expr) {
	in := []rune(code)
	parser := NewParser(NewLexer(in))
	result := parser.parsePrimaryExpr()
	if eq, msg := compareExpr(result, expected); !eq {
		fmt.Printf("Error, expected:\n   %#v\nbut got:\n   %#v\nMSG: %s\n", expected, result, msg)
		t.Fail()
	}
}

func TestPrimaryExpr(t *testing.T) {
	testPrimaryExpr(t, "test", &Ident{expr{0}, "test"})
	testPrimaryExpr(t, "test(arg)", &FuncCall{expr: expr{4}, Left: &Ident{expr{0}, "test"}, Args: nil}) // TODO: fill args
	testPrimaryExpr(t, "test.tere[123]",
		&ArrayExpr{
			expr: expr{9},
			Left: &DotSelector{
				expr:  expr{4},
				Left:  &Ident{expr{0}, "test"},
				Right: &Ident{expr{5}, "tere"}},
			Index: &BasicLit{expr{10}, &Token{TOKEN_NUM, 10, 123}}})
}

func testExpr(t *testing.T, code string, expected Expr) {
	in := []rune(code)
	parser := NewParser(NewLexer(in))
	result := parser.parseExpr()
	if eq, msg := compareExpr(result, expected); !eq {
		fmt.Printf("Error, expected:\n   %#v\nbut got:\n   %#v\nMSG: %s\n", expected, result, msg)
		t.Fail()
	}
}

func TestParseExpr(t *testing.T) {
	testExpr(t, "test+pies", &BinaryOp{
		expr:  expr{4},
		Left:  &Ident{expr{0}, "test"},
		Right: &Ident{expr{5}, "pies"},
	})
	testExpr(t, "1*2+3*4", &BinaryOp{
		expr: expr{3},
		Left: &BinaryOp{
			expr:  expr{1},
			Left:  &BasicLit{expr: expr{0}}, // TODO: put num value
			Right: &BasicLit{expr: expr{2}}, // TODO: put num value
		},
		Right: &BinaryOp{
			expr:  expr{5},
			Left:  &BasicLit{expr: expr{4}}, // TODO: put num value
			Right: &BasicLit{expr: expr{6}}, // TODO: put num value
		},
	})
}
