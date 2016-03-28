package have

import (
	"fmt"
	"reflect"
	"strings"
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
	parser.dontLookup = true
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
	testPrimaryExpr(t, "test", &Ident{expr{0}, "test", nil})
	testPrimaryExpr(t, "test(arg)", &FuncCallExpr{expr: expr{4}, Left: &Ident{expr{0}, "test", nil}, Args: nil}) // TODO: fill args
	testPrimaryExpr(t, "test.tere[123]",
		&ArrayExpr{
			expr: expr{9},
			Left: &DotSelector{
				expr:  expr{4},
				Left:  &Ident{expr{0}, "test", nil},
				Right: &Ident{expr{5}, "tere", nil}},
			Index: &BasicLit{expr{10}, nil, &Token{TOKEN_NUM, 10, 123}}})
	testPrimaryExpr(t, "{1,2}", &CompoundLit{})
	testPrimaryExpr(t, "{1:2}.bla", &DotSelector{expr: expr{5},
		Left:  &CompoundLit{expr: expr{0}},
		Right: &Ident{expr{6}, "bla", nil}})
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
	parser.dontLookup = true
	result, err := parser.parseExpr()
	if eq, msg := compareExpr(result, expected); !eq || err != nil {
		fmt.Printf("Error, expected:\n   %#v\nbut got:\n   %#v\nMSG: %s\n", expected, result, msg)
		t.Fail()
	}
}

func TestParseExpr(t *testing.T) {
	testExpr(t, "test+pies", &BinaryOp{
		expr:  expr{4},
		Left:  &Ident{expr{0}, "test", nil},
		Right: &Ident{expr{5}, "pies", nil},
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
	parser.dontLookup = true
	result, err := parser.parseType()
	if err != nil {
		fmt.Print(err)
		t.Fail()
	}
	if result.String() != expected.String() {
		fmt.Printf("Types aren't equal (%s and %s)\n", result.String(), expected.String())
		t.Fail()
	}
}

func TestParseType(t *testing.T) {
	testTypes(t, "int", &SimpleType{ID: simpleTypeStrToID["int"]})
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
	parser.dontLookup = true
	result, err := parser.parseArgs(0)
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
	testArgs(t, "1,bla", []Expr{&BasicLit{expr{0}, nil, &Token{TOKEN_NUM, 0, "1"}},
		&Ident{expr{2}, "bla", nil}})
	testArgs(t, "1,bla)", []Expr{&BasicLit{expr{0}, nil, &Token{TOKEN_NUM, 0, "1"}},
		&Ident{expr{2}, "bla", nil}})
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

func TestForStmt(t *testing.T) {
	cases := []struct {
		code       string
		shouldPass bool
	}{
		{`for i = 0; i < 10; i+1:
	var y = 3`, true},
		{`for i = 0; j < 10; i+1:
	var y = 3`, false},
		{`for i = 0; i < 10; j+1:
	var y = 3`, false},
		{`for i = 0; ; i+1:
	var y = 3`, true},
		{`for i = 0; i < 10; :
	var y = 3`, true},
		{`for ;;:
	var y = 3`, true},
	}

	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.parseForStmt(nil)

		passed := err == nil

		if passed != c.shouldPass {
			t.Fail()
			fmt.Printf("Error parsing `for`, shouldPass=%v, %s %s\n", c.shouldPass, err, spew.Sdump(result))
		}
	}
}

func TestIfStmt(t *testing.T) {
	cases := []struct {
		code       string
		shouldPass bool
	}{
		{`if x = 0; x == 1:
	var y = 3`, true},
		{`if x = 100; x > 10:
  if true:
    var y = 3`, true},
		{`if x int; false:
  var y = 3`, true},
		{`if x int; false:
  var y = 3
else:
  var x = 3`, true},
		{`if x int; false:
  var y = 3
  else:
    var x = 3`, false},
		{`if x int; false:
  var y = 3
else:`, false},
		{`if x int; false:
  var y = 3
else:
var x = 3`, false},
		{`if x int; false:
  var y = 3
else
  var x = 3`, false},
	}
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.parseIf()

		passed := err == nil

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if passed != c.shouldPass {
			t.Fail()
			fmt.Printf("Error parsing `if`, shouldPass=%v, %s %s\n", c.shouldPass, err, spew.Sdump(result))
		}
	}
}

func TestParseInlineStruct(t *testing.T) {
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
		result, err := parser.parseStruct(nil)

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if err != nil {
			t.Fail()
			fmt.Printf("Error parsing `struct` %s %s\n", err, spew.Sdump(result))
		}
	}
}

func TestParseStructStmt(t *testing.T) {
	cases := []string{
		`struct A:
  x int
  yb string

`,
		`struct A:
  x int
  kreff struct:
    pr int
  yb string`,
		`
struct Abc:
	func foo():
		pass`,
		`
struct Abc:
	x int
	y string
	func foo():
		pass
	func* bar():
		pass
	z int`,
	}
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(strings.TrimSpace(c))))
		result, err := parser.parseStructStmt()

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
		parser.dontLookup = true
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

func TestIdentSearch(t *testing.T) {
	cases := []struct {
		code  string
		valid bool
	}{
		{`func abc(x int):
		  var x = 1
		`, true},
		{`func abc(x int) int:
		  var zzz = 1
		  var y = x * zzz
		`, true},
		{`func abc() int:
		  var x = z
		`, false},
		{`func abc(x int, y int) int:
	if z = 1; z == x:
		var b = x * y * z
`, true},
		{`func abc(x int, y int) int:
	if z = 1; z == x:
		var b = x * y * z
	var a = x * y * z
`, false},
		{`func abc(x int, y int) int:
	if Z = 1; z == x:
		var b = x * y * z
`, false},
		{`func abc(x int) int:
	if y = 1; y == x:
		if z = 1; z == y:
			var b = x * y * z
`, true},
	}
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.parseFunc()

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if c.valid && err != nil {
			t.Fail()
			fmt.Printf("Error parsing %s %s\n", err, spew.Sdump(result))
		} else if err == nil && !c.valid {
			t.Fail()
			fmt.Printf("Parsing should've failed %s %s\n", err, spew.Sdump(result))
		}
	}
}

type validityTestCase struct {
	code  string
	valid bool
}

func validityTest(t *testing.T, cases []validityTestCase) {
	for _, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.Parse()

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if c.valid && err != nil {
			t.Fail()
			fmt.Printf("Error parsing a function %s %s\n", err, spew.Sdump(result))
		} else if err == nil && !c.valid {
			t.Fail()
			fmt.Printf("Parsing a compound literal should've failed %s %s\n", err, spew.Sdump(result))
		}
	}
}

func TestFuncDecl(t *testing.T) {
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
		{`func abc(x int, y int) int:
		  var x = y * 2
		`, true},
		{`func abc(x, y int) int:
		  var x = 1
		`, true},
		{`func abc(x, y int, z int = 5) int:
		  var x = 1
		`, true},
		{`func abc() int, float64:
  var x = 1
`, true},
		{`func abc() (x int):
		  var x = 1
`, true},
		{`func abc() int, struct:
    x int
    y float64:
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
			fmt.Printf("Error parsing a function %s %s\n", err, spew.Sdump(result))
		} else if err == nil && !c.valid {
			t.Fail()
			fmt.Printf("Parsing a compound literal should've failed %s %s\n", err, spew.Sdump(result))
		}
	}
}

func TestSimpleStmt(t *testing.T) {
	cases := []validityTestCase{
		{`var x = 1
x = 2`, true},
		{`var x, y = 1, 2
y, x = 1, 2`, true},
		{`var x, y = 1, 2
y, x, y = 1, 2`, false},
		{`var x, y = 1, 2
y, x`, false},
		{`var x = 1
x += 1`, true},
		{`var x, y = 1, 2
x, y += 1, 2`, false},
	}
	validityTest(t, cases)
}

func TestParseInterfaces(t *testing.T) {
	cases := []validityTestCase{
		{`
type Ble interface:
	func x()
`, true},
		{`
type Ble interface:
	func x()
`, true},
		{`
type Ble interface:
	func *x()
`, true},
		{`
type Ble interface:
	x int
	func x()
`, false},
		{`
interface Ble:
	func x()
`, true},
		{`
interface:
	func x()
`, false},
		{`
type Ble interface:
	x int
	func x()
`, false},
	}
	validityTest(t, cases)
}

func TestBranchStmt(t *testing.T) {
	cases := []validityTestCase{
		{`break`, true},
		{`continue`, true},
		{`
func x():
	for x = 0; x < 10; x += 1:
		break`, true},
		{`
func x():
	break`, false},
		{`
func x():
	lol:
	goto lol`, true},
		{`
func x():
	lol:
	lol:
	goto lol`, false},
		{`
func x():
	if true:
		break
	for x = 0; x < 10; x += 1:
		pass`, false},
		{`
func x():
	goto lol`, false},
		{`
func x():
	lol:
	for x = 0; x < 10; x += 1:
		goto lol`, true},
		{`
func x():
	goto lol
	for x = 0; x < 10; x += 1:
		lol:`, false},
		{`
func x():
	lol:
	pass
	for x = 0; x < 10; x += 1:
		break lol`, false},
		{`
func x():
	lol:
	for x = 0; x < 10; x += 1:
		break lol`, true},
	}
	validityTest(t, cases)
}

func TestVarDecl(t *testing.T) {
	var cases = []struct {
		code     string
		expected *VarStmt
	}{
		{"var x int\n", &VarStmt{
			stmt: stmt{expr: expr{}},
			Vars: DeclChain{&VarDecl{
				Vars: []*Variable{
					&Variable{
						name: "x",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
					},
				},
				Inits: nil,
			}},
		}},
		{
			"var x int = 1 + 2\n",
			&VarStmt{
				stmt: stmt{expr: expr{}},
				Vars: DeclChain{&VarDecl{Vars: []*Variable{
					&Variable{
						name: "x",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
					},
				},
					Inits: []Expr{
						&BinaryOp{
							expr: expr{pos: 14},
							Left: &BasicLit{
								expr:  expr{pos: 12},
								token: &Token{Type: TOKEN_NUM, Offset: 12, Value: "1"},
							},
							Right: &BasicLit{
								expr:  expr{pos: 16},
								token: &Token{Type: TOKEN_NUM, Offset: 16, Value: "2"},
							},
							op: &Token{Type: TOKEN_PLUS, Offset: 14, Value: "+"},
						},
					},
				},
				},
			},
		},

		{
			"var x,y int = 1, 2\n",
			&VarStmt{
				stmt: stmt{expr: expr{pos: 0}},
				Vars: DeclChain{&VarDecl{Vars: []*Variable{
					&Variable{
						name: "x",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
					},
					&Variable{
						name: "y",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
					},
				},
					Inits: []Expr{
						&BasicLit{
							expr:  expr{pos: 14},
							token: &Token{Type: TOKEN_NUM, Offset: 14, Value: "1"},
						},
						&BasicLit{
							expr:  expr{pos: 17},
							token: &Token{Type: TOKEN_NUM, Offset: 17, Value: "2"},
						},
					},
				}},
			},
		},
		{
			"var x,y int = (1, 2), z = 3\n",

			&VarStmt{
				stmt: stmt{},
				Vars: []*VarDecl{
					&VarDecl{
						Vars: []*Variable{
							&Variable{
								name: "x",
								Type: &SimpleType{ID: 1},
							},
							&Variable{
								name: "y",
								Type: &SimpleType{ID: 1},
							},
						},
						Inits: []Expr{
							&BasicLit{
								expr: expr{pos: 15},
								typ:  nil,
								token: &Token{
									Type:   13,
									Offset: 15,
									Value:  "1",
								},
							},
							&BasicLit{
								expr: expr{pos: 18},
								typ:  nil,
								token: &Token{
									Type:   13,
									Offset: 18,
									Value:  "2",
								},
							},
						},
					},
					&VarDecl{
						Vars: []*Variable{
							&Variable{
								name: "z",
								Type: &UnknownType{},
							},
						},
						Inits: []Expr{
							&BasicLit{
								expr: expr{pos: 26},
								typ:  nil,
								token: &Token{
									Type:   13,
									Offset: 26,
									Value:  "3",
								},
							},
						},
					},
				},
				IsFuncStmt: false,
			},
		},
		{
			"var x,y int = (1), 2\n",
			&VarStmt{
				stmt: stmt{expr: expr{}},
				Vars: DeclChain{&VarDecl{Vars: []*Variable{
					&Variable{
						name: "x",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
					},
					&Variable{
						name: "y",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
					},
				},
					Inits: []Expr{
						&BasicLit{
							expr: expr{pos: 15},
							token: &Token{
								Type:   TOKEN_NUM,
								Offset: 15,
								Value:  "1",
							},
						},
						&BasicLit{
							expr: expr{pos: 19},
							token: &Token{
								Type:   TOKEN_NUM,
								Offset: 19,
								Value:  "2",
							},
						},
					},
				},
				},
			},
		},
		{
			"var x int, y = 1\n",

			&VarStmt{
				stmt: stmt{},
				Vars: []*VarDecl{
					&VarDecl{
						Vars: []*Variable{
							&Variable{
								name: "x",
								Type: &SimpleType{ID: 1},
							},
						},
						Inits: nil,
					},
					&VarDecl{
						Vars: []*Variable{
							&Variable{
								name: "y",
								Type: &UnknownType{},
							},
						},
						Inits: []Expr{
							&BasicLit{
								expr: expr{pos: 15},
								typ:  nil,
								token: &Token{
									Type:   13,
									Offset: 15,
									Value:  "1",
								},
							},
						},
					},
				},
				IsFuncStmt: false,
			},
		},
	}

	for i, test := range cases {
		in := []rune(test.code)
		parser := NewParser(NewLexer(in))
		result, err := parser.parseVarStmt(true)
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
