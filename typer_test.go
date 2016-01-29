package have

import (
	"fmt"
	"testing"
)

func TestWithLookups(t *testing.T) {
	var cases = []struct {
		code       string
		shouldPass bool
		typ        string
	}{
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
var a point = {x: 1, y: 2}`,
			true,
			"point",
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
	}

	for i, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.Parse()
		if err != nil {
			t.Fail()
			fmt.Printf("FAIL: Failed parsing: %s\n", err)
		}

		var varStmt *VarStmt = nil
		var ok = false

		for _, stmt := range result {
			varStmt, ok = stmt.(*VarStmt)
			if ok {
				err = varStmt.NegotiateTypes()
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
			firstVar := varStmt.Vars[0]
			if firstVar.Type.String() != c.typ || !IsAssignable(firstVar.Init.(TypedExpr).Type(), firstVar.Type) {
				t.Fail()
				fmt.Printf("FAIL: Case %d: Bad type: %s, %s, %s\n", i, c.typ, firstVar.Type.String(),
					firstVar.Init.(TypedExpr).Type().String())
			}
		}
	}
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
		result, err := parser.parseVarStmt()
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
			firstVar := result.Vars[0]
			if firstVar.Type.String() != c.typ || firstVar.Init.(TypedExpr).Type().String() != c.typ {
				t.Fail()
				fmt.Printf("Case %d: Bad type: %s, %s, %s\n", i, c.typ, firstVar.Type.String(),
					firstVar.Init.(TypedExpr).Type().String())
			}
		}
	}
}
