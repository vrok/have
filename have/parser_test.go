package have

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
	"unicode"

	gotoken "go/token"

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
			if unicode.IsLower(rune(valA.Type().Field(i).Name[0])) {
				continue
			}
			if field.Interface() == nil &&
				valB.Field(i).Interface() == nil {
				continue
			}
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

func newTestParser(code string) *Parser {
	in := []rune(code)
	fs := gotoken.NewFileSet()
	l := NewLexer(in, fs.AddFile("a.go", fs.Base(), len(in)), 0)
	return NewParser(l)
}

func testPrimaryExpr(t *testing.T, code string, expected Expr) {
	parser := newTestParser(code)
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
	testPrimaryExpr(t, "test", &Ident{expr{1}, "test", nil, false})
	testPrimaryExpr(t, "te_st", &Ident{expr{1}, "te_st", nil, false})
	testPrimaryExpr(t, "_test", &Ident{expr{1}, "_test", nil, false})
	testPrimaryExpr(t, "test(arg)", &FuncCallExpr{expr: expr{5}, Left: &Ident{expr{1}, "test", nil, false}, Args: nil}) // TODO: fill args
	testPrimaryExpr(t, "test.tere[123]",
		&ArrayExpr{
			expr: expr{10},
			Left: &DotSelector{
				expr:  expr{5},
				Left:  &Ident{expr{1}, "test", nil, false},
				Right: &Ident{expr{6}, "tere", nil, false}},
			Index: []Expr{&BasicLit{expr{10}, &Token{TOKEN_INT, 10, 123, 1}}}})
	testPrimaryExpr(t, "dywan[1:5]", &ArrayExpr{
		expr: expr{6},
		Left: &Ident{expr{1}, "dywan", nil, false},
		Index: []Expr{&SliceExpr{expr: expr{7},
			From: &BasicLit{expr{6}, &Token{TOKEN_INT, 6, 1, 1}},
			To:   &BasicLit{expr{8}, &Token{TOKEN_INT, 8, 5, 1}},
		}},
	})
	testPrimaryExpr(t, "{1,2}", &CompoundLit{expr: expr{1}})
	testPrimaryExpr(t, "{1:2}.bla", &DotSelector{expr: expr{6},
		Left:  &CompoundLit{expr: expr{1}},
		Right: &Ident{expr{7}, "bla", nil, false}})
	testPrimaryExpr(t, "map[int]int{1:2}", &CompoundLit{expr: expr{1}, Left: &TypeExpr{expr: expr{1}}})
	testPrimaryExpr(t, "[]int{1,2}", &CompoundLit{expr: expr{1}, Left: &TypeExpr{expr: expr{1}}})
	testPrimaryExpr(t, "dywan{1}", &CompoundLit{expr: expr{1}, Left: &TypeExpr{expr: expr{1}}})
	//testPrimaryExpr(t, "dy.wan{1}", &CompoundLit{})
	testPrimaryExpr(t, `struct {
    x int }
	{x: 1}`, &CompoundLit{expr: expr{1}, Left: &TypeExpr{expr: expr{1}}})
	testPrimaryExpr(t, `struct {
    x int }
  {x: 1}`, &CompoundLit{expr: expr{1}, Left: &TypeExpr{expr: expr{1}}})
	testPrimaryExpr(t, `struct {
    x int
}
	  {x: 1}`, &CompoundLit{expr: expr{1}, Left: &TypeExpr{expr: expr{1}}})
	testPrimaryExpr(t, `struct {
	x int
}
{x: 1}`, &CompoundLit{expr: expr{1}, Left: &TypeExpr{expr: expr{1}}})
}

func testExpr(t *testing.T, code string, expected Expr) {
	parser := newTestParser(code)
	parser.dontLookup = true
	result, err := parser.parseExpr()
	if eq, msg := compareExpr(result, expected); !eq || err != nil {
		fmt.Printf("Error, expected:\n   %#v\nbut got:\n   %#v\nMSG: %s\n", expected, result, msg)
		t.Fail()
	}
}

func TestParseExpr(t *testing.T) {
	testExpr(t, "test+pies", &BinaryOp{
		expr:  expr{5},
		Left:  &Ident{expr{1}, "test", nil, false},
		Right: &Ident{expr{6}, "pies", nil, false},
	})
	testExpr(t, "1*2+3*4", &BinaryOp{
		expr: expr{4},
		Left: &BinaryOp{
			expr:  expr{2},
			Left:  &BasicLit{expr: expr{1}}, // TODO: put num value
			Right: &BasicLit{expr: expr{3}}, // TODO: put num value
		},
		Right: &BinaryOp{
			expr:  expr{6},
			Left:  &BasicLit{expr: expr{5}}, // TODO: put num value
			Right: &BasicLit{expr: expr{7}}, // TODO: put num value
		},
	})
	testExpr(t, "(1+2)*(3+4)", &BinaryOp{
		expr: expr{6},
		Left: &BinaryOp{
			expr:  expr{3},
			Left:  &BasicLit{expr: expr{2}}, // TODO: put num value
			Right: &BasicLit{expr: expr{4}}, // TODO: put num value
		},
		Right: &BinaryOp{
			expr:  expr{9},
			Left:  &BasicLit{expr: expr{8}},  // TODO: put num value
			Right: &BasicLit{expr: expr{10}}, // TODO: put num value
		},
	})
}

func testTypes(t *testing.T, code string, expected Type) {
	parser := newTestParser(code)
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
	parser := newTestParser(code)
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
	testArgs(t, "1,bla", []Expr{&BasicLit{expr{1}, &Token{TOKEN_INT, 0, "1", 1}},
		&Ident{expr{3}, "bla", nil, false}})
	testArgs(t, "1,bla)", []Expr{&BasicLit{expr{1}, &Token{TOKEN_INT, 0, "1", 1}},
		&Ident{expr{3}, "bla", nil, false}})
}

func TestCodeBlock(t *testing.T) {
	var cases = []struct {
		code       string
		stmtCounts int
	}{
		{`
 var x = 1
 var y = 2
}`, 2},
		{`
 var x = 1
}
var y = 2`, 1},
	}
	for _, c := range cases {
		parser := newTestParser(c.code)
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
		{`for var i = 0; i < 10; i+1 {
	var y = 3
}`, true},
		{`for var i = 0; j < 10; i+1 {
	var y = 3
}`, false},
		{`for var i = 0; i < 10; j+1 {
	var y = 3
}`, false},
		{`for var i = 0; ; i+1 {
	var y = 3
}`, true},
		{`for var i = 0; i < 10; {
	var y = 3
}`, true},
		{`for ;; {
	var y = 3
}`, true},
		{`for var i range {1, 2, 3} {
	var y = 3
}`, true},
		{`for var i, j range {1, 2, 3} {
	var y = 3
}`, true},
		{`for var i int range {1, 2, 3} {
	var y = 3
} `, false},
		{`for true {
	pass
}`, true},
		{`for true { pass }`, true},
		{`for var x = 0 {
	pass
}`, false},
	}

	for i, c := range cases {
		if *justCase >= 0 && i != *justCase {
			continue
		}

		parser := newTestParser(c.code)
		result, err := parser.parseForStmt(nil)

		passed := (err == nil && len(parser.unboundIdents) == 0)

		if passed != c.shouldPass {
			t.Fail()
			fmt.Printf("Error parsing `for`, shouldPass=%v, %s %s\n", c.shouldPass, err, result)
		}
	}
}

func TestIfStmt(t *testing.T) {
	cases := []struct {
		code       string
		shouldPass bool
	}{
		{`if var x = 0; x == 1 {
	var y = 3
}`, true},
		{`if var x int = 0; x == 1 {
	var y = 3
}`, true},
		{`if var x = 100; x > 10 {
  if true {
    var y = 3
  }
}`, true},
		{`if var x = 100; x > 10 { if true { var y = 3 }}`, true},
		{`if var x = 100; x > 10 {
	if true { var y = 3 }}`, true},
		{`if var x = 100; x > 10 { if true {
	var y = 3 }}`, true},
		{`if var x = 100; x > 10 { if true {`, false},
		{`if var x = 1; false {
  var y = 3
}`, true},
		{`if var x = 1; false {
  var y = 3
} else {
  var x = 3
}`, true},
		{`if x int; false {
  var y = 3
} else {`, false},
		{`if x = 1; false {
  var y = 3
} else
var x = 3`, false},
	}
	for i, c := range cases {
		if *justCase >= 0 && i != *justCase {
			continue
		}

		parser := newTestParser(c.code)
		result, err := parser.parseIf()

		passed := (err == nil && len(parser.unboundIdents) == 0)

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if passed != c.shouldPass {
			t.Fail()
			fmt.Printf("Error parsing `if`, shouldPass=%v, %s %s\n", c.shouldPass, err, spew.Sdump(result))
		}
	}
}

func TestParseSwitchStmt(t *testing.T) {
	cases := []validityTestCase{
		{`switch 7 {
case 7:
	pass
}`, true},
		{`switch 7 {
{ case 7:
	pass
}
}`, false},
		{`switch 7 {
case 7, 8, 9:
	pass
}`, true},
		{`switch 7 {
case 7:
	pass
default:
	pass
}`, true},
		{`switch var x = 1; x {
case 7:
	pass
}`, true},
		{`var x int
switch x = 1; x {
case 7:
	pass
}`, true},
		// Only `=` assignment allowed
		{`var x int
switch x += 1; x {
case 7:
	pass
}`, false},
		{`switch x = 1; x {
case 7:
	pass
}`, false},
		{`switch {
case 1 == 1:
	pass
}`, true},
	}
	validityTest(t, cases)
}

func TestParseInlineStruct(t *testing.T) {
	cases := []string{
		`struct {
  x int
  yb string
}`,
		`struct { x int }`,
		`struct
{ x int }`,
		`struct { pass }`,
		`struct {
  x int
  kreff struct {
    pr int
  }
  yb string
}`,
	}
	for _, c := range cases {
		parser := newTestParser(c)
		result, err := parser.parseStruct(nil, false)

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
		`struct A {
  x int
  yb string
}`,
		`struct A { x int }`,
		`struct A
{ x int }`,
		`struct A { x int
}`,
		`struct A { pass }`,
		`struct A { func a() { pass }}`,
		`struct A {
  x int
  kreff struct {
    pr int
  }
  yb string
}`,
		`
struct Abc {
	func foo() {
		pass
	}
}`,
		`
struct Abc {
	x int
	y string
	func foo() {
		pass
	}
	func* bar() {
		pass
	}
	z int
}`,
	}
	for i, c := range cases {
		if *justCase >= 0 && i != *justCase {
			continue
		}

		parser := newTestParser(strings.TrimSpace(c))
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
		{`{{1: "a"}}`, true, COMPOUND_LISTLIKE},
		{`{{1: "a",}}`, true, COMPOUND_LISTLIKE},
		{`{{1,}}`, true, COMPOUND_LISTLIKE},
	}
	for _, c := range cases {
		parser := newTestParser(c.code)
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
	cases := []validityTestCase{
		{`func abc(x int) {
		  var x = 1
}`, true},
		{`func abc(x int) int {
		  var zzz = 1
		  var y = x * zzz
}`, true},
		{`func abc() int {
		  var x = z
}`, false},
		{`func abc(x int, y int) int {
	if var z = 1; z == x {
		var b = x * y * z
	}
}`, true},
		{`func abc(x int, y int) int {
	var z = 2
	if z = 1; z == x {
		var b = x * y * z
	}
}`, true},
		{`func abc(x int, y int) int {
	if var z = 1; z == x {
		var b = x * y * z
	}
	var a = x * y * z
}`, false},
		{`func abc(x int, y int) int {
	if var Z = 1; z == x {
		var b = x * y * z
	}
}`, false},
		{`func abc(x int) int {
	if var y = 1; y == x {
		if var z = 1; z == y {
			var b = x * y * z
		}
	}
}`, true},
	}
	for _, c := range cases {
		parser := newTestParser(c.code)
		result, _, err := parser.parseFunc(false)

		passed := (err == nil && len(parser.unboundIdents) == 0)

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if c.valid && !passed {
			t.Fail()
			fmt.Printf("Error parsing a function %s %s\n", err, spew.Sdump(result))
		} else if !c.valid && passed {
			t.Fail()
			fmt.Printf("Parsing a compound literal should've failed %s %s\n", err, spew.Sdump(result))
		}
	}
}

type validityTestCase struct {
	code  string
	valid bool
}

func validityTest(t *testing.T, cases []validityTestCase) {
	for i, c := range cases {
		if *justCase >= 0 && i != *justCase {
			continue
		}
		parser := newTestParser("\n" + c.code)

		block, err := parser.parseUnindentedBlock()

		passed := (err == nil && (len(parser.unboundIdents)+len(parser.unboundTypes) == 0))

		// TODO: better assertions, more test cases.
		// We'll need something more succint than comparing whole ASTs.
		if c.valid && !passed {
			t.Fail()
			fmt.Printf("Error parsing %s %s\n", err, spew.Sdump(block))
			fmt.Printf("Case %d: Unbound idents and types: [%s], [%s]\n", i, spew.Sdump(parser.unboundIdents), spew.Sdump(parser.unboundTypes))
		} else if !c.valid && passed {
			t.Fail()
			fmt.Printf("Parsing case %d should've failed (err: %s)\n%s\n---\n%s\n",
				i, err, c.code, spew.Sdump(block))
		}
	}
}

func TestParseFuncDecl(t *testing.T) {
	cases := []struct {
		code  string
		valid bool
	}{
		{`func abc(x int) { var x = 1 }`, true},
		{`func abc(x int) int {
		  var x = 1
}`, true},
		{`func abc() int {
		  var x = 1
  }`, true},
		{`func abc(x int, y int) int {
		  var x = y * 2
  }	`, true},
		{`func abc(x, y int) int {
		  var x = 1
  }		`, true},
		{`func abc(x, y int, z string) int {
	pass }`, true},
		{`func abc(x, y int, z) int { # Error: last parameter needs type
	pass }`, false},
		{`func abc(x, y int int, z string) int { # Error: unexpected token (double int)
	pass }`, false},
		{`func abc() (int, float64) {
  var x = 1
}`, true},
		{`func abc() (x int) {
		  var x = 1
 } `, true},
		{`func abc() (int, struct {
    x int
    y float64}) {
  var x = 1
}`, true},
	}
	for _, c := range cases {
		parser := newTestParser(c.code)
		result, _, err := parser.parseFunc(false)

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

func TestNakedControlClauses(t *testing.T) {
	// This tests a very specific issue which is caused by Go-like syntax.
	// Check comments around nakedControlClause var for more information.

	cases := []validityTestCase{
		{`for var a range {1, 2, 3} { pass }`, true},
		{`for var a range []int{1, 2, 3} { pass }`, true},
		{`type List []int		
for var a range List{1, 2, 3} { pass }`, false},
		{`type List []int		
for var a range (List{1, 2, 3}) { pass }`, true},
		{`type Point [2]int
var p = Point{1, 2}
if p == Point{0, 0} { pass }`, false},
		{`type Point [2]int
var p = Point{1, 2}
if p == (Point{0, 0}) { pass }`, true},
	}
	validityTest(t, cases)
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
type Ble interface {
	func x()
}`, true},
		{`
type Ble interface {
	func x(a, b int)
}`, true},
		{`
type Ble interface {
	func x(a, b int, c string)
}`, true},
		{`
type Ble interface {
	func x(int, string)
}`, true},
		{`
type Ble interface {
	func x(a int, string)
}`, false},
		{`
type Ble interface {
	func x() int
}`, true},
		{`
type Ble interface {
	func *x()
}`, true},
		{`
type Ble interface {
	x int
	func x()
}`, false},
		{`
interface Ble{
	func x()
}`, true},
		{`
interface{
	func x()
}`, false},
		{`
type Ble interface{
	x int
	func x()
}`, false},
		{`
type Ble interface{
	func x()
}`, true},
		{`
type Ble interface{
	func x()
	func y()
}`, true},
		{`
type Ble interface{
	pass
}`, true},
		{`
type Ble interface{
}`, true},
		{`type Ble interface{}`, true},
		{`type Ble interface
{}`, true},
	}
	validityTest(t, cases)
}

func TestParseChannels(t *testing.T) {
	cases := []validityTestCase{
		{`var x chan int`, true},
		{`var x <-chan int`, true},
		{`var x chan<- int`, true},
		{`var x <-chan<- int`, false},
		{`var x chan`, false},
		{`var x chan chan int`, true},
	}
	validityTest(t, cases)
}

func TestParseSendExpr(t *testing.T) {
	cases := []validityTestCase{
		{`var x chan int
x <- 7`, true},
		{`var x chan int
x <-`, false},
		{`var x chan int
<- 7`, true},
		{`var x chan int
var y = <-x`, true},
	}
	validityTest(t, cases)
}

func TestParseTypeAssertion(t *testing.T) {
	cases := []validityTestCase{
		{`var x = "bla".(string)`, true},
		{`var x = "bla".(type)`, true},
		{`var x = "bla".(nil)`, false},
		{`var x = 123
var y = x.(float32)`, true},
		{`var x = 123
var y, z = x.(float32)`, true},
		{`var x = 123
var y = x.()`, false},
	}
	validityTest(t, cases)
}

func TestParseReturnStmt(t *testing.T) {
	cases := []validityTestCase{
		{`func a() { return 1 }`, true},
		{`func a() { return 1, "piesek" }`, true},
		{`func a() {
	return
}`, true},
		{`func a() { return }`, true},
		{`return`, false},
	}
	validityTest(t, cases)
}

func TestBranchStmt(t *testing.T) {
	cases := []validityTestCase{
		{`break`, true},
		{`continue`, true},
		{`
func x() {
	for x = 0; x < 10; x += 1 {
		break
	}
}`, true},
		{`
func x() {
	break
}`, false},
		{`
func x() {
	lol:
	goto lol
}`, true},
		{`
func x() {
	lol:
	lol:
	goto lol
}`, false},
		{`
func x() {
	if true {
		break
	}
	for x = 0; x < 10; x += 1 {
		pass
	}
}`, false},
		{`
func x() {
	goto lol
}`, false},
		{`
func x() {
	lol:
	for x = 0; x < 10; x += 1 {
		goto lol
	}
}`, true},
		{`
func x() {
	goto lol
	for x = 0; x < 10; x += 1 {
		lol:
	}
}`, false},
		{`
func x() {
	lol:
	pass
	for x = 0; x < 10; x += 1 {
		break lol
	}
}`, false},
		{`
func x() {
	lol:
	for x = 0; x < 10; x += 1 {
		break lol
	}
}`, true},
	}
	validityTest(t, cases)
}

func TestParseGenericFunc(t *testing.T) {
	cases := []validityTestCase{
		{`func a[T]() int { # OK
	return 1
}`, true},
		{`func a[T, K]() int { # OK
	return 1
}`, true},
		{`func a[]() int { # Expected a generic type name
	return 1
}`, false},
		{`func a[T]() T { # OK
	return 1
}`, true},
		// TODO: the sample below causes a runtime panic instead of an error
		{`func a[T]() K { # K is unknown
	return 1
}`, false},
		{`func a[T](x T) T { # some recursion
	return a(x + 1)
}`, true},
	}
	validityTest(t, cases)
}

func TestParseGenericStruct(t *testing.T) {
	cases := []validityTestCase{
		{`struct A[T] {
	pass
}`, true},
		{`struct A[T] { pass }`, true},
		{`struct A[T, K] {
	pass
}`, true},
		{`struct A[T, K] {
	func x() T {
		return T{}
	}
}`, true},
		{`struct A[T] {
	func x() K { # K is unknown
		return 1}}}`, false},
		// This would be a cool test if validityTest allowed hanging idents:
		//		{`struct A[T]:
		//	func x() A[T]:
		//		return self`, true},
		//		{`struct A[T]:
		//	pass
		//var x A[int]`, true},
	}
	validityTest(t, cases)
}

func TestParseWhenStmt(t *testing.T) {
	cases := []validityTestCase{
		{`
func Bla[X]() {
	when X {
	is int:
		pass
	}
}`, true},
		{`
func Bla[X]() {
	when X is int {
		pass
	}
	pass
}`, true},
		{`
func Bla[X]() {
	when X {
	is int:
		pass
	default:
		pass
	}
	pass
}`, true},
		{`
func Bla[X]() {
	when X {
	default:
		pass
	is int: # 'default' has to be the last branch
		pass
	}
	pass
}`, false},
		{`
func Bla[X, Y, Z]() {
	when Z, Y, X {
	is int, string, implements interface{}:
		pass
	}
}`, true},
		{`
func Bla[X, Y, Z]() {
	when Z, Y, X is int, string, implements interface{} {
		pass
	}
}`, true},
		{`
func Bla[X, Y, Z]() {
	when Z, Y, X {
	is int, string, implements interface{}:
		pass
	implements interface{}, interface{}, is string:
		pass
	}
}`, true},
		{`
func Bla[X, Y, Z]() {
	when Z, Y, X {
	is int, string, implements:
		pass
	}
}`, false},
	}
	validityTest(t, cases)
}

func TestVarDecl(t *testing.T) {
	var cases = []struct {
		code     string
		expected *VarStmt
	}{
		{"var x int\n", &VarStmt{
			stmt: stmt{expr: expr{1}},
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
				stmt: stmt{expr: expr{1}},
				Vars: DeclChain{&VarDecl{Vars: []*Variable{
					&Variable{
						name: "x",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
						init: &BinaryOp{
							expr: expr{pos: 15},
							Left: &BasicLit{
								expr:  expr{pos: 13},
								token: &Token{Type: TOKEN_INT, Offset: 12, Value: "1", Pos: 13},
							},
							Right: &BasicLit{
								expr:  expr{pos: 17},
								token: &Token{Type: TOKEN_INT, Offset: 16, Value: "2", Pos: 17},
							},
							op: &Token{Type: TOKEN_PLUS, Offset: 14, Value: "+", Pos: 15},
						},
					},
				},
					Inits: []Expr{
						&BinaryOp{
							expr: expr{pos: 15},
							Left: &BasicLit{
								expr:  expr{pos: 13},
								token: &Token{Type: TOKEN_INT, Offset: 12, Value: "1", Pos: 13},
							},
							Right: &BasicLit{
								expr:  expr{pos: 17},
								token: &Token{Type: TOKEN_INT, Offset: 16, Value: "2", Pos: 17},
							},
							op: &Token{Type: TOKEN_PLUS, Offset: 14, Value: "+", Pos: 15},
						},
					},
				},
				},
			},
		},

		{
			"var x,y int = 1, 2\n",
			&VarStmt{
				stmt: stmt{expr: expr{pos: 1}},
				Vars: DeclChain{&VarDecl{Vars: []*Variable{
					&Variable{
						name: "x",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
						init: &BasicLit{
							expr:  expr{pos: 15},
							token: &Token{Type: TOKEN_INT, Offset: 14, Value: "1", Pos: 15},
						},
					},
					&Variable{
						name: "y",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
						init: &BasicLit{
							expr:  expr{pos: 18},
							token: &Token{Type: TOKEN_INT, Offset: 17, Value: "2", Pos: 18},
						},
					},
				},
					Inits: []Expr{
						&BasicLit{
							expr:  expr{pos: 15},
							token: &Token{Type: TOKEN_INT, Offset: 14, Value: "1", Pos: 15},
						},
						&BasicLit{
							expr:  expr{pos: 18},
							token: &Token{Type: TOKEN_INT, Offset: 17, Value: "2", Pos: 18},
						},
					},
				}},
			},
		},
		{
			"var x,y int = (1, 2), z = 3\n",

			&VarStmt{
				stmt: stmt{expr: expr{1}},
				Vars: []*VarDecl{
					&VarDecl{
						Vars: []*Variable{
							&Variable{
								name: "x",
								Type: &SimpleType{ID: SIMPLE_TYPE_INT},
								init: &BasicLit{
									expr: expr{pos: 16},
									token: &Token{
										Type:   13,
										Offset: 15,
										Value:  "1",
										Pos:    16,
									},
								},
							},
							&Variable{
								name: "y",
								Type: &SimpleType{ID: SIMPLE_TYPE_INT},
								init: &BasicLit{
									expr: expr{pos: 19},
									token: &Token{
										Type:   13,
										Offset: 18,
										Value:  "2",
										Pos:    19,
									},
								},
							},
						},
						Inits: []Expr{
							&BasicLit{
								expr: expr{pos: 16},
								token: &Token{
									Type:   13,
									Offset: 15,
									Value:  "1",
									Pos:    16,
								},
							},
							&BasicLit{
								expr: expr{pos: 19},
								token: &Token{
									Type:   13,
									Offset: 18,
									Value:  "2",
									Pos:    19,
								},
							},
						},
					},
					&VarDecl{
						Vars: []*Variable{
							&Variable{
								name: "z",
								Type: &UnknownType{},
								init: &BasicLit{
									expr: expr{pos: 27},
									token: &Token{
										Type:   13,
										Offset: 26,
										Value:  "3",
										Pos:    27,
									},
								},
							},
						},
						Inits: []Expr{
							&BasicLit{
								expr: expr{pos: 27},
								token: &Token{
									Type:   13,
									Offset: 26,
									Value:  "3",
									Pos:    27,
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
				stmt: stmt{expr: expr{1}},
				Vars: DeclChain{&VarDecl{Vars: []*Variable{
					&Variable{
						name: "x",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
						init: &BasicLit{
							expr: expr{pos: 16},
							token: &Token{
								Type:   TOKEN_INT,
								Offset: 15,
								Value:  "1",
								Pos:    16,
							},
						},
					},
					&Variable{
						name: "y",
						Type: &SimpleType{ID: simpleTypeStrToID["int"]},
						init: &BasicLit{
							expr: expr{pos: 20},
							token: &Token{
								Type:   TOKEN_INT,
								Offset: 19,
								Value:  "2",
								Pos:    20,
							},
						},
					},
				},
					Inits: []Expr{
						&BasicLit{
							expr: expr{pos: 16},
							token: &Token{
								Type:   TOKEN_INT,
								Offset: 15,
								Value:  "1",
								Pos:    16,
							},
						},
						&BasicLit{
							expr: expr{pos: 20},
							token: &Token{
								Type:   TOKEN_INT,
								Offset: 19,
								Value:  "2",
								Pos:    20,
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
				stmt: stmt{expr: expr{1}},
				Vars: []*VarDecl{
					&VarDecl{
						Vars: []*Variable{
							&Variable{
								name: "x",
								Type: &SimpleType{ID: SIMPLE_TYPE_INT},
							},
						},
						Inits: nil,
					},
					&VarDecl{
						Vars: []*Variable{
							&Variable{
								name: "y",
								Type: &UnknownType{},
								init: &BasicLit{
									expr: expr{pos: 16},
									token: &Token{
										Type:   13,
										Offset: 15,
										Value:  "1",
										Pos:    16,
									},
								},
							},
						},
						Inits: []Expr{
							&BasicLit{
								expr: expr{pos: 16},
								token: &Token{
									Type:   13,
									Offset: 15,
									Value:  "1",
									Pos:    16,
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
		parser := newTestParser(test.code)
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
