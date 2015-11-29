package have

import (
	"fmt"
	"reflect"
	"testing"

	spew "github.com/davecgh/go-spew/spew"
	"github.com/kr/pretty"
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
				field.Interface().(Expr),
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
	result, err := parser.parsePrimaryExpr()
	if err != nil {
		fmt.Printf("Parsing primary expr failed: %s", err)
		t.Fail()
	}
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
	testPrimaryExpr(t, "{1,2}", &CompoundLit{})
	testPrimaryExpr(t, "{1:2}.bla", &DotSelector{expr: expr{5},
		Left:  &CompoundLit{expr: expr{0}},
		Right: &Ident{expr{6}, "bla"}})
	testPrimaryExpr(t, "map[int]int{1:2}", &CompoundLit{})
	testPrimaryExpr(t, "[]int{1,2}", &CompoundLit{})
	testPrimaryExpr(t, "dywan{1}", &CompoundLit{})
	//testPrimaryExpr(t, "dy.wan{1}", &CompoundLit{})
	testPrimaryExpr(t, `struct:
    x int
	{x: 1}`, &CompoundLit{})
	testPrimaryExpr(t, `struct:
    x int
  {x: 1}`, &CompoundLit{})
	testPrimaryExpr(t, `struct:
    x int
	  {x: 1}`, &CompoundLit{})
}

func testExpr(t *testing.T, code string, expected Expr) {
	in := []rune(code)
	parser := NewParser(NewLexer(in))
	result, err := parser.parseExpr()
	if eq, msg := compareExpr(result, expected); !eq || err != nil {
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
	testExpr(t, "(1+2)*(3+4)", &BinaryOp{
		expr: expr{5},
		Left: &BinaryOp{
			expr:  expr{2},
			Left:  &BasicLit{expr: expr{1}}, // TODO: put num value
			Right: &BasicLit{expr: expr{3}}, // TODO: put num value
		},
		Right: &BinaryOp{
			expr:  expr{8},
			Left:  &BasicLit{expr: expr{7}}, // TODO: put num value
			Right: &BasicLit{expr: expr{9}}, // TODO: put num value
		},
	})
}

func testTypes(t *testing.T, code string, expected Type) {
	in := []rune(code)
	parser := NewParser(NewLexer(in))
	result, err := parser.parseType()
	if err != nil {
		fmt.Print(err)
		t.Fail()
	}
	if result.String() != expected.String() {
		fmt.Printf("Types aren't equal (%d and %d)\n", result.String(), expected.String())
		t.Fail()
	}
}

func TestParseType(t *testing.T) {
	testTypes(t, "int", &SimpleType{Name: "int"})
	testTypes(t, "trelemorele", &CustomType{Name: "trelemorele"})
	testTypes(t, "*trelemorele", &PointerType{To: &CustomType{Name: "trelemorele"}})
	testTypes(t, "**trelemorele", &PointerType{To: &PointerType{To: &CustomType{Name: "trelemorele"}}})
	testTypes(t, "[123]trelemorele", &ArrayType{Of: &CustomType{Name: "trelemorele"}, Size: 123})
	testTypes(t, "*[123]trelemorele", &PointerType{To: &ArrayType{Of: &CustomType{Name: "trelemorele"}, Size: 123}})
	testTypes(t, "[]trelemorele", &SliceType{Of: &CustomType{Name: "trelemorele"}})
}

func testArgs(t *testing.T, code string, expected []Expr) {
	in := []rune(code)
	parser := NewParser(NewLexer(in))
	result, err := parser.parseArgs()
	if err != nil {
		fmt.Print(err)
		t.Fail()
	}
	if len(result) != len(expected) {
		fmt.Printf("Lengths are different (%d and %d)\n", len(result), len(expected))
		t.Fail()
	}
	for i, arg := range result {
		equal, msg := compareExpr(arg, expected[i])
		if !equal {
			fmt.Printf(msg)
			t.Fail()
		}
	}
}

func TestArgs(t *testing.T) {
	testArgs(t, "", []Expr{})
	testArgs(t, ")", []Expr{})
	testArgs(t, "1,bla", []Expr{&BasicLit{expr{0}, &Token{TOKEN_NUM, 0, "1"}},
		&Ident{expr{2}, "bla"}})
	testArgs(t, "1,bla)", []Expr{&BasicLit{expr{0}, &Token{TOKEN_NUM, 0, "1"}},
		&Ident{expr{2}, "bla"}})
}

func TestCodeBlock(t *testing.T) {
	var cases = []struct {
		code       string
		stmtCounts int
	}{
		{`
 var x = 1
 var y = 2`, 2},
		{`
 var x = 1
var y = 2`, 1},
	}
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		//parser.nextToken()
		result, err := parser.parseCodeBlock()

		if err != nil {
			fmt.Printf("Parsing block failed: %s", err)
			t.Fail()
		} else if len(result.Statements) != c.stmtCounts {
			fmt.Printf("Wrong number of statements in code block, %s, %s", err, spew.Sdump(result))
			t.Fail()
		}
	}
}

/*
func TestIfStmt(t *testing.T) {
	cases := []string{
		`if var x = 0; x == 1:
	var y = 3`,
		`if var x = 100; x > 10:
  if true:
    var y = 3`,
		`if var x int; false:
  var y = 3`,
	}
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c)))
		result, err := parser.parseIf()

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if err != nil {
			t.Fail()
			fmt.Printf("Error parsing `if` %s %s\n", err, spew.Sdump(result))
		}
	}
}*/

func TestParseStruct(t *testing.T) {
	cases := []string{
		`struct:
  x int
  yb string

`,
		`struct:
  x int
  kreff struct:
    pr int
  yb string`,
	}
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c)))
		result, err := parser.parseStruct()

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if err != nil {
			t.Fail()
			fmt.Printf("Error parsing `struct` %s %s\n", err, spew.Sdump(result))
		}
	}
}

func TestParseCompoundLiterals(t *testing.T) {
	cases := []struct {
		code  string
		valid bool
		kind  CompoundLitKind
	}{
		{`{1, 2, 3}`, true, COMPOUND_LISTLIKE},
		{`{1: 2, 3: 4}`, true, COMPOUND_MAPLIKE},
		{`{a: "123", b: 4}`, true, COMPOUND_MAPLIKE},
		{`{a+b: "123", (b*2): false}`, true, COMPOUND_MAPLIKE},
		{`{
 a: "123",
         b: 4
                   }`, true, COMPOUND_MAPLIKE},
		{`{1: 2, 3}`, false, COMPOUND_UNKNOWN},
		{`{1: 2, 3, 4: 5}`, false, COMPOUND_UNKNOWN},
		{`{2, 3: 4}`, false, COMPOUND_UNKNOWN},
		{`{1}`, true, COMPOUND_LISTLIKE},
	}
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.parseCompoundLit()

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if c.valid && (err != nil || c.kind != result.kind) {
			t.Fail()
			fmt.Printf("Error parsing a compound literal %s %s\n", err, spew.Sdump(result))
		} else if err == nil && !c.valid {
			t.Fail()
			fmt.Printf("Parsing a compound literal should've failed %s %s\n", err, spew.Sdump(result))
		}
	}
}

func disabled_TestFuncDecl(t *testing.T) {
	cases := []struct {
		code  string
		valid bool
	}{
		{`func abc(x int):
  var x = 1
`, true},
		{`func abc(x int) int:
  var x = 1
`, true},
		{`func abc() int:
  var x = 1
`, true},
	}
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.parseFunc()

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if c.valid && err != nil {
			t.Fail()
			fmt.Printf("Error parsing a compound literal %s %s\n", err, spew.Sdump(result))
		} else if err == nil && !c.valid {
			t.Fail()
			fmt.Printf("Parsing a compound literal should've failed %s %s\n", err, spew.Sdump(result))
		}
	}
}

func TestVarDecl(t *testing.T) {
	var cases = []struct {
		code     string
		expected *VarStmt
	}{
		{"var x int\n", &VarStmt{
			expr: expr{},
			Vars: []*VarDecl{
				&VarDecl{
					Name: "x",
					Type: &SimpleType{Name: "int"},
					Init: &BlankExpr{},
				},
			},
		}},
		{
			"var x int = 1 + 2\n",
			&VarStmt{
				expr: expr{},
				Vars: []*VarDecl{
					&VarDecl{
						Name: "x",
						Type: &SimpleType{Name: "int"},
						Init: &BinaryOp{
							expr: expr{pos: 14},
							Left: &BasicLit{
								expr:  expr{pos: 12},
								token: &Token{Type: TOKEN_NUM, Offset: 12, Value: "1"},
							},
							Right: &BasicLit{
								expr:  expr{pos: 16},
								token: &Token{Type: TOKEN_NUM, Offset: 16, Value: "2"},
							},
							op: &Token{Type: TOKEN_PLUS, Offset: 14, Value: nil},
						},
					},
				},
			},
		},

		{
			"var x,y int = 1, 2\n",
			&VarStmt{
				expr: expr{pos: 0},
				Vars: []*VarDecl{
					&VarDecl{
						Name: "x",
						Type: &SimpleType{Name: "int"},
						Init: &BasicLit{
							expr:  expr{pos: 14},
							token: &Token{Type: TOKEN_NUM, Offset: 14, Value: "1"},
						},
					},
					&VarDecl{
						Name: "y",
						Type: &SimpleType{Name: "int"},
						Init: &BasicLit{
							expr:  expr{pos: 17},
							token: &Token{Type: TOKEN_NUM, Offset: 17, Value: "2"},
						},
					},
				},
			},
		},
		/*
			{
				"var x,y int, z string = 1, 2, \"bum\"\n",
				&VarStmt{
					expr: expr{},
					Vars: []*VarDecl{
						&VarDecl{
							Name: "x",
							Type: &SimpleType{Name: "int"},
							Init: &BasicLit{
								expr:  expr{pos: 24},
								token: &Token{Type: TOKEN_NUM, Offset: 24, Value: "1"},
							},
						},
						&VarDecl{
							Name: "y",
							Type: &SimpleType{Name: "int"},
							Init: &BasicLit{
								expr:  expr{pos: 27},
								token: &Token{Type: TOKEN_NUM, Offset: 27, Value: "2"},
							},
						},
						&VarDecl{
							Name: "z",
							Type: &SimpleType{Name: "string"},
							Init: &BasicLit{
								expr:  expr{pos: 30},
								token: &Token{Type: TOKEN_STR, Offset: 30, Value: "bum"},
							},
						},
					},
				},
			},*/
		/*
			{
				"var x,y int = 1, 2, z = 3\n",
				&VarStmt{},
			},
		*/
		{
			"var x,y int = (1, 2), z = 3\n",
			&VarStmt{},
		},
		{
			"var x,y int = (1), 2\n",
			&VarStmt{},
		},
		{
			"var x int, y = 1\n",
			&VarStmt{},
		},
	}

	for i, test := range cases {
		in := []rune(test.code)
		parser := NewParser(NewLexer(in))
		result, err := parser.parseVarStmt()
		if err != nil {
			fmt.Printf("Case #%d error: %s\n", i+1, err)
			t.Fail()
		}
		if !reflect.DeepEqual(result, test.expected) {
			fmt.Printf("Case #%d error, got: %# v, but wanted: %# v\n",
				i+1, pretty.Formatter(result), pretty.Formatter(test.expected))
			t.Fail()
		}
	}
}
