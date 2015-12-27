package have

import (
	"fmt"
	"testing"
)

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
	}

	for i, c := range cases {
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.parseVarStmt()
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
