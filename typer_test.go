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
	}

	for i, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.Parse()
		if err != nil {
			t.Fail()
			fmt.Printf("Failed parsing: %s\n", err)
		}

		var varStmt *VarStmt = nil

		for _, stmt := range result {
			varStmt = stmt.(*VarStmt)
			err = varStmt.NegotiateTypes()
			if err != nil {
				break
			}
		}

		if (err == nil) != c.shouldPass {
			t.Fail()
			fmt.Printf("Case %d: Bad code accepted or good code parsed with an error for '%s'\nError: %s\n",
				i, c.code, err)
			return
		}

		if c.shouldPass {
			firstVar := varStmt.Vars[0]
			if firstVar.Type.String() != c.typ || firstVar.Init.(TypedExpr).Type().String() != c.typ {
				t.Fail()
				fmt.Printf("Case %d: Bad type: %s, %s, %s\n", i, c.typ, firstVar.Type.String(),
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
		fmt.Printf("DOING CASE ---\n%s\n---\n", c.code)
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
