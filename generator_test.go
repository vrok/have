package have

import (
	"fmt"
	"strings"
	"testing"
)

type generatorTestCase struct {
	source, reference string
}

func inMemTranspile(code string) (string, error) {
	parser := NewParser(NewLexer([]rune(code)))
	result, err := parser.Parse()
	if err != nil {
		return "", err
	}

	for _, stmt := range result {
		typedStmt := stmt.(ExprToProcess)
		if err := typedStmt.NegotiateTypes(); err != nil {
			return "", err
		}
	}

	cc := &CodeChunk{}

	for _, stmt := range result {
		stmt.(Generable).Generate(cc)
	}

	return cc.ReadAll(), nil
}

func testCases(t *testing.T, cases []generatorTestCase) {
	for i, c := range cases {
		result, err := inMemTranspile(c.source)
		if err != nil {
			t.Fail()
			fmt.Printf("Error compiling case %d\nError: %s\nCode: %s\n", i, err, c.source)
		}
		if a, b := strings.TrimSpace(result), strings.TrimSpace(c.reference); a != b {
			t.Fail()
			fmt.Printf("Different input and output for case %d.\nInput: `%s`\nOutput: `%s`\nWanted: `%s`\n",
				i, c.source, a, b)
		}
	}
}

func TestGenerateExpr(t *testing.T) {
	cases := []generatorTestCase{
		{source: "1", reference: "1\n"},
		{source: "\"bla\"", reference: "\"bla\"\n"},
		{source: "true", reference: "true\n"},
		{source: "false", reference: "false\n"},
		{source: "'a'", reference: "'a'\n"},
		{source: "'ą'", reference: "'ą'\n"},
		{source: "1+1", reference: "(1 + 1)\n"},
		{source: "1+(-1)", reference: "(1 + (-1))\n"},
		{source: "func a():\n 1", reference: "func a() {\n\t1\n}\n"},
		{source: "func a(x, y int):\n 1", reference: "func a(x int, y int) {\n\t1\n}\n"},
		{source: "print(\"test\")", reference: "print(\"test\")\n"},
		{source: "if 1 == 2:\n 1", reference: `
if (1 == 2) {
	1
}`,
		},
		{source: "if t = 1; t == 2:\n 1", reference: `
if t := (int)(1); (t == 2) {
	1
}`,
		},
		{source: "if t = 1, k = \"aaa\"; t == 2 && k == \"bbb\":\n 1", reference: `
if t, k := (int)(1), (string)("aaa"); ((t == 2) && (k == "bbb")) {
	1
} `,
		},
		{source: "if 1 == 2:\n 1\nelse:\n 2\n", reference: `
if (1 == 2) {
	1
} else {
	2
}`},
		{source: "if 1 == 2:\n 1\nelif true == false:\n 5\nelse:\n 2\n", reference: `
if (1 == 2) {
	1
} else if (true == false) {
	5
} else {
	2
}`},
		{source: `for x = 0; x < 100; print("a"):
	print("b")`, reference: `for x := (int)(0); (x < 100); print("a") {
	print("b")
}`},
		{source: `for x = 0; x < 100; print("a"):
	break`, reference: `for x := (int)(0); (x < 100); print("a") {
	break
}`},
		{source: `
goto bla
bla:`, reference: `goto bla
bla:
`},
		{source: `
struct A:
	x int
	func setX(z int):
		self.x = z
	func *setY(z string):
		self.y = z
	y string
`, reference: `type A struct {
	x int
	y string
}

func (self A) setX(z int) {
	self.x = z
}

func (self *A) setY(z string) {
	self.y = z
}`},
		{source: `
var x = 1`, reference: `var x = (int)(1)`},
		{source: `
var x = map[int]string {
	1: "bla",
	2: "ble"
}`, reference: `var x = (map[int]string)(map[int]string{
	1: "bla",
	2: "ble",
})`},
		{source: `
var x = []string{"ble", "bla"}`, reference: `
var x = ([]string)([]string{
	"ble",
	"bla",
})`},
		{source: `
var x = []string{}`, reference: `
var x = ([]string)([]string{})`},
		{source: `
var j = struct:
	x int
	y string
	{ 0, "ble" }`, reference: `
var j = (struct {x int; y string})(struct {x int; y string}{
	0,
	"ble",
})`},
		{source: `
var j = struct:
	x int
	y string
{ 0, "ble" }`, reference: `
var j = (struct {x int; y string})(struct {x int; y string}{
	0,
	"ble",
})`},
		{source: `var x *int = nil`,
			reference: `var x = (*int)(nil)`},
		{source: `
interface A:
	func a() int
var x A = nil`,
			reference: `
type A interface{a() int}
var x = (A)(nil)`},
		{source: `var x map[int]string
var y = x[1]
y = x[1]`,
			reference: `
var x = (map[int]string)(nil)
var y = (string)(x[1])
y = x[1]
`},
		{source: `var x string = "blah"
var y = x[1]
y = x[1]`,
			reference: `
var x = (string)("blah")
var y = (byte)(x[1])
y = x[1]
`},
		{source: `var x []string
var y = x[1:4]
y = x[1:4]`,
			reference: `
var x = ([]string)(nil)
var y = ([]string)(x[1:4])
y = x[1:4]
`},
		{source: `func a() int,
		string:
	return 1,
		"bla"`, reference: `func a() (int, string) {
	return 1, "bla"
}`},
	}
	testCases(t, cases)
}

func TestMapIndexTupleAssign(t *testing.T) {
	cases := []generatorTestCase{
		{source: `var x map[string]int
var a, ok = x["ech"]
a, ok = x["ech"]`,
			reference: `
var x = (map[string]int)(nil)
var a, ok = x["ech"]
a, ok = x["ech"]
`},

		{source: `
type A map[string]int		
var x A
var a, ok = x["ech"]
a, ok = x["ech"]`,
			reference: `
type A map[string]int
var x = (A)(nil)
var a, ok = x["ech"]
a, ok = x["ech"]
`},
	}
	testCases(t, cases)
}

func TestGenerateChannels(t *testing.T) {
	cases := []generatorTestCase{
		{source: `var x chan int`,
			reference: `var x = (chan int)(nil)`},
		{source: `var x chan<- int`,
			reference: `var x = (chan<- int)(nil)`},
		{source: `var x <-chan int`,
			reference: `var x = (<-chan int)(nil)`},
		{source: `var x chan int
var b = <-x`,
			reference: `var x = (chan int)(nil)
var b = (int)((<-x))`},
		{source: `var x chan int
var a, b = <-x
a, b = <-x`,
			reference: `var x = (chan int)(nil)
var a, b = (<-x)
a, b = (<-x)`},
	}
	testCases(t, cases)
}

func TestGenerateReturnStmts(t *testing.T) {
	cases := []generatorTestCase{
		{source: `func a() int:
	return 7`,
			reference: `func a() (int) {
	return 7
}`},
		{source: `func a() int, string:
	return 7, "ble"`,
			reference: `func a() (int, string) {
	return 7, "ble"
}`},
		{source: `
struct A:
	x int
func a() *A:
	return &A{x: 10}`,
			reference: `type A struct {
	x int
}

func a() (*A) {
	return (&A{
		x: 10,
	})
}`},
	}
	testCases(t, cases)
}

func TestGenerateUninitializedVar(t *testing.T) {
	cases := []generatorTestCase{
		{source: `var x map[int]string`,
			reference: `var x = (map[int]string)(nil)`},
		{source: `var x int`,
			reference: `var x = (int)(0)`},
		{source: `var x string`,
			reference: `var x = (string)("")`},
		{source: `var x bool`,
			reference: `var x = (bool)(false)`},
		{source: `var x [3]int`,
			reference: `var x = ([3]int)([3]int{0, 0, 0})`},
		{source: `var x [3][2]int`,
			reference: `var x = ([3][2]int)([3][2]int{[2]int{0, 0}, [2]int{0, 0}, [2]int{0, 0}})`},
		{source: `var x struct:
	y int`, reference: `var x = (struct {y int})(struct {y int}{})`},

		{source: `struct A:
	y int
var x A`, reference: `
type A struct {
	y int
}

var x = (A)(struct {y int}{})
`},
	}
	testCases(t, cases)
}

func TestGenerateTypeAssertions(t *testing.T) {
	cases := []generatorTestCase{
		{source: `interface A:
	func x()
struct B:
	func x():
		pass
var x A
var y = x.(B)`,
			reference: `type A interface{x()}
type B struct {
}

func (self B) x() {
	// pass
}

var x = (A)(nil)
var y = (B)(x.(B))`},
		{source: `interface A:
	func x()
struct B:
	func x():
		pass
var x A
var y, z = x.(B)`,
			reference: `type A interface{x()}
type B struct {
}

func (self B) x() {
	// pass
}

var x = (A)(nil)
var y, z = x.(B)`},
	}
	testCases(t, cases)
}

func TestGenerateSwitchStmt(t *testing.T) {
	cases := []generatorTestCase{
		{source: `switch 7
case 7:
	pass
case 1, 2, 3:
	pass`,
			reference: `switch 7 {
case 7:
	// pass
case 1, 2, 3:
	// pass
}`},
		{source: `switch var x = 1; x + 2
case 1, 2, 3:
	pass`,
			reference: `switch x := (int)(1); (x + 2) {
case 1, 2, 3:
	// pass
}`},
		{source: `switch
case true || false:
	pass
default:
	print("a")`,
			reference: `switch  {
case (true || false):
	// pass
default:
	print("a")
}`},
		{source: `
func apply(l []int, f func(x int) int) []int:
	pass
var l = apply({1, 2, 3}, func(x int) int:
	return x + 2)
`,
			reference: `func apply(l []int, f func(int) int) ([]int) {
	// pass
}
var l = ([]int)(apply([]int{
	1,
	2,
	3,
}, func (x int) (int) {
	return (x + 2)
}))`},
	}
	testCases(t, cases)
}

func TestGenerateTypeSwitchStmt(t *testing.T) {
	cases := []generatorTestCase{
		{source: `
var bla interface:
	func a()
struct x:
	func a():
		pass
switch bla.(type)
case x: # Error: impossible assertion, x doesn't implement the interface
	pass
`,
			reference: `
var bla = (interface{a()})(nil)
type x struct {
}

func (self x) a() {
	// pass
}

switch bla.(type) {
case x:
	// pass
}
`},
	}
	testCases(t, cases)
}

func TestGenerateNestedBlocks(t *testing.T) {
	cases := []generatorTestCase{
		{source: `
func a():
	func b():
		func c():
			func d():
				pass
	func e():
		pass
`,
			reference: `func a() {
	func b() {
		func c() {
			func d() {
				// pass
			}
		}
	}
	func e() {
		// pass
	}
}`},
	}
	testCases(t, cases)
}
