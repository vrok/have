package have

import (
	"fmt"
	"strings"
	"testing"
)

type typeTestCase struct {
	code       string
	shouldPass bool
	typ        string
}

func testVarTypes(t *testing.T, cases []typeTestCase) {
	for i, c := range cases {
		parser := NewParser(NewLexer([]rune(strings.TrimSpace(c.code))))
		result, err := parser.Parse()
		if err != nil {
			t.Fail()
			fmt.Printf("FAIL: Failed parsing: %s\n", err)
		}

		var stmtWithTypes ExprToProcess = nil
		var ok = false

		for _, stmt := range result {
			stmtWithTypes, ok = stmt.(ExprToProcess)
			if ok {
				err = stmtWithTypes.NegotiateTypes()
				if err != nil {
					break
				}
			}
		}

		if (err == nil) != c.shouldPass {
			t.Fail()
			fmt.Printf("FAIL: Case %d: Bad code accepted or good code parsed with an error for '%s'\nError: %s\n",
				i, c.code, err)
			return
		}

		if c.shouldPass {
			firstDecl := stmtWithTypes.(*VarStmt).Vars[0]

			firstVar, firstInit := firstDecl.Vars[0], Expr(nil)
			if len(firstDecl.Inits) > 0 {
				firstInit = firstDecl.Inits[0]
			}

			if firstVar.Type.String() != c.typ || !IsAssignable(firstInit.(TypedExpr).Type(), firstVar.Type) {
				t.Fail()
				fmt.Printf("FAIL: Case %d: Bad type: %s, %s, %s\n", i, c.typ, firstVar.Type.String(),
					firstInit.(TypedExpr).Type().String())
			}
		}
	}
}

func TestWithLookups(t *testing.T) {
	var cases = []typeTestCase{
		{`var b int = 2		
var a int = b`,
			true,
			"int",
		},
		{`var b = 2
var a int = b`,
			true,
			"int",
		},
		{`var b = 2
var a int = b * 2`,
			true,
			"int",
		},
		{`var b int = 2, c int = 30, d int = 40
var a int = b * c + d + 10`,
			true,
			"int",
		},
		{`var b = 2, c = 30, d = 40
var a int = b * c + d + 10`,
			true,
			"int",
		},
		{`type number int
var a number = 1`,
			true,
			"number",
		},
		{`type text string
var a text = "trele morele"`,
			true,
			"text",
		},
		{`type number int
var a number = "trele morele"`,
			false,
			"",
		},
		{`type text string
type scribble text
var a scribble= "trele morele"`,
			true,
			"scribble",
		},
		{`type pint *int
var x = 1
var a pint = &x`,
			true,
			"pint",
		},
		{`type text string
var x string = "1"
var a text = x`,
			false,
			"",
		},
		{`type hash map[int]string
var b hash = {1: "aaa"}
var a hash = b`,
			true,
			"hash",
		},
		{`type hash map[int]string
var b hash = {1: "aaa"}
var a map[int]string = b`,
			true,
			"map[int]string",
		},
		{`type hash map[int]string
var b map[int]string = {1: "aaa"}
var a hash = b`,
			true,
			"hash",
		},
		{`type chain []string
var b []string = {"aaa"}
var a chain = b`,
			true,
			"chain",
		},
		{`type hash map[int]string
var b hash = {1: "aaa"}
var a map[string]string = b`,
			false,
			"",
		},
		{`var b, c, d = 2, 30, 40
var a int = b * c + d + 10`,
			true,
			"int",
		},
		{`var b = struct:
	x int
var y int = b.x`,
			true,
			"int",
		},
		{`var b = struct:
	x int
var y = b.x`,
			true,
			"int",
		},
		{`var b = struct:
	c struct:
		d string
var f = b.c.d`,
			true,
			"string",
		},
		{`var b = ((*struct:
	c string)(&{c: "ech"}))
var f = b.c`,
			true,
			"string",
		},
		{`var b = ((int)(1))`,
			true,
			"int",
		},
		{`var b = struct:
	x int
var y string = b.x`,
			false,
			"",
		},
		{`var b int = 2		
var a string = b`,
			false,
			"",
		},
		{`var b = 2		
var a string = b`,
			false,
			"",
		},
		{`func f() int:
	var x = 1
var a int = f()`,
			true,
			"int",
		},
		{`func f() int:
	var x int = "a"
var a = f()`,
			false,
			"",
		},
		{`func f() string:
	var x = 1
var a int = f()`,
			false,
			"",
		},
		{`func f():
	var x = 1
var a int = f()`,
			false,
			"",
		},
		{`func f(x int) int:
	var x = 1
var a int = f(4)`,
			true,
			"int",
		},
		{`func f(x string) int:
	var x = 1
var a int = f(4)`,
			false,
			"",
		},
		{`func f(x string, y int) int:
	var x = 1
var b int = 5
var a int = f("las", b)`,
			true,
			"int",
		},
		{`func f(x string, y int) int:
	var x = 1
var b string = "5"
var a int = f("las", b)`,
			false,
			"",
		},
		{`func f() int:
	if true:
		var y = 2
	var x = 1
var a int = f()`,
			true,
			"int",
		},
		{`func f() int:
	if 1:
		var y = 2
	var x = 1
var a int = f()`,
			false,
			"",
		},
		{`var x = true || false`,
			true,
			"bool",
		},
		{`var x = 1 || 2`,
			false,
			"",
		},
		{`func f() int:
	for x = 0; x < 100; print("a"):
		var y = 2
var a int = f()`,
			true,
			"int",
		},
		{`func f() int:
	for x = 0; x < 100; print(1):
		pass
var a int = f()`,
			false,
			"int",
		},
		{`func f() int:
	for ;;:
		pass
var a int = f()`,
			true,
			"int",
		},
		{`func f():
	var x = 1
	x = 2`,
			true,
			"func()",
		},
		{`func f():
	var x = 1
	x = "bla"`,
			false,
			"",
		},
	}

	testVarTypes(t, cases)
}

func TestCustomStructTypes(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`type point struct:
	x int
	y int
var a point = {}
var b point = a`,
			true,
			"point",
		},
		{`type point struct:
	x int
	y int
var a point = {1, 2}
var b point = a`,
			true,
			"point",
		},
		{`type point struct:
	x int
	y int
var a = point{1, 2}`,
			true,
			"point",
		},
		{`type point struct:
	x int
	y int
var a point = {x: 1, y: 2}`,
			true,
			"point",
		},
		{`type point struct:
	x int
var a = point{}
var b = a.x`,
			true,
			"int",
		},
		{`type point struct:
	x int
	y int
var a point = {x: 1, z: 2}`,
			false,
			"",
		},
		{`type point struct:
	x int
	y int
var a point = {x: 1, y: "2"}`,
			false,
			"",
		},
		{`type point struct:
	x int
	y int
var a struct:
	x int
	y int = {}
var b point = a`,
			true,
			"point",
		},
	})
}

func TestTypesIfStmt(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`func f() int:
	if 1 == 2:
		var y = 2
	var x = 1
var a int = f()`,
			true,
			"int",
		},
	})
}

func TestTypesStruct(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
struct Abc:
	func x():
		pass
var a = Abc{}`,
			true,
			"Abc",
		},
		{`
struct Abc:
	x int
	func x():
		pass
var a = Abc{x: 7}`,
			true,
			"Abc",
		},
		{`
struct Abc:
	x int
	func x():
		pass
var a = Abc{y: 7}`,
			false,
			"Abc",
		},
		{`
struct Abc:
	func x(z int):
		z = 3
		pass
var a = Abc{}`,
			true,
			"Abc",
		},
		{`
struct Abc:
	zz int
	func x(z int):
		self.zz = z
	func *xp(z int):
		self.zz = z
		(*self).zz = z
var a = Abc{}`,
			true,
			"Abc",
		},
	})
}

func TestSimple(t *testing.T) {
	var cases = []struct {
		code       string
		shouldPass bool
		typ        string
	}{
		{`var a int = 1`,
			true,
			"int",
		},
		{`var a *int = &1`,
			true,
			"*int",
		},
		{`var a int = *&1`,
			true,
			"int",
		},
		{`var a = &*&1`,
			true,
			"*int",
		},
		{`var a *int = *1`,
			false,
			"",
		},
		{`var a = &1`,
			true,
			"*int",
		},
		{`var a string = "reksio"`,
			true,
			"string",
		},
		{`var a bool = true`,
			true,
			"bool",
		},
		{`var a bool = false`,
			true,
			"bool",
		},
		{`var a = true`,
			true,
			"bool",
		},
		{`var a = "blabla"`,
			true,
			"string",
		},
		{`var a = 123`,
			true,
			"int",
		},
		{`var a string = 1`,
			false,
			"", // whatever, shouldn't pass anwyay
		},
		{`var a int = "123"`,
			false,
			"", // whatever, shouldn't pass anwyay
		},
		{`var a bool = 0`,
			false,
			"", // whatever, shouldn't pass anwyay
		},
		{
			`var a []int = {1, 2, 3}`,
			true,
			"[]int",
		},
		{
			`var a [3]int = {1, 2, 3}`,
			true,
			"[3]int",
		},
		{
			`var a [2]int = {1, 2, 3}`,
			false,
			"",
		},
		{
			`var a []int = {1, "2", 3}`,
			false,
			"",
		},
		{
			`var a []int = {}`,
			true,
			"[]int",
		},
		{
			`var a [][]int = {{1}, {1,2}, {}}`,
			true,
			"[][]int",
		},
		{
			`var a [][]int = {{1}, {1,2}, {"1"}}`,
			false,
			"",
		},
		{
			`var a = {1, 2, 3}`,
			true,
			"[]int",
		},
		{
			`var a = {"bla", "2", "3"}`,
			true,
			"[]string",
		},
		{
			`var a = {"bla", "2", 3}`,
			false,
			"",
		},
		{
			`var a = {{"bla", "2"}, {"3"}}`,
			true,
			"[][]string",
		},
		{
			`var a = {{"bla", "2"}, {3}}`,
			false,
			"",
		},
		{
			`var a map[int]string = {1: "a", 2: "b"}`,
			true,
			"map[int]string",
		},
		{
			`var a = {1: "a", 2: "b"}`,
			true,
			"map[int]string",
		},
		{
			`var a = {1: {"a"}, 2: {"b", "c"}}`,
			true,
			"map[int][]string",
		},
		{
			`var a map[int]string = {1: "a", "2": "b"}`,
			false,
			"",
		},
		{
			`var a map[int]string = {1: "a", 2: 3}`,
			false,
			"",
		},
		{
			`var a int = 2 + 2`,
			true,
			"int",
		},
		{
			`var a = 2 + 2`,
			true,
			"int",
		},
		{
			`var a int = 2 + {1}`,
			false,
			"",
		},
		{
			`var a = 2 + {1}`,
			false,
			"",
		},
		{
			`var a int = +2`,
			true,
			"int",
		},
		{
			`var a = +2`,
			true,
			"int",
		},
	}

	for i, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.parseVarStmt(true)
		if err != nil {
			t.Fail()
			fmt.Printf("Case %d: Failed parsing: %s\n", i, err)
		}
		err = result.Vars[0].NegotiateTypes()

		if (err == nil) != c.shouldPass {
			t.Fail()
			fmt.Printf("Case %d: Bad code accepted or good code parsed with an error for '%s'\nError: %s\n",
				i, c.code, err)
			continue
		}

		if err == nil {
			firstDecl := result.Vars[0]

			firstVar, firstInit := firstDecl.Vars[0], Expr(nil)
			if len(firstDecl.Inits) > 0 {
				firstInit = firstDecl.Inits[0]
			}

			if firstVar.Type.String() != c.typ || firstInit.(TypedExpr).Type().String() != c.typ {
				t.Fail()
				fmt.Printf("Case %d: Bad type: %s, %s, %s\n", i, c.typ, firstVar.Type.String(),
					firstInit.(TypedExpr).Type().String())
			}
		}
	}
}
