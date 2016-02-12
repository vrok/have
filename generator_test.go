package have

import (
	"fmt"
	"testing"
)

type generateTestCase struct {
	code       string
	shouldPass bool
	typ        string
}

func testVarGenerate(t *testing.T, cases []generateTestCase) {
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

			cc := &CodeChunk{}
			varStmt.Generate(cc)
			fmt.Printf("ZZZ \n\n--------\n%s\n--------\n\n", cc.ReadAll())
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

func TestGenerate(t *testing.T) {
	var cases = []generateTestCase{
		{`var b int = 2		
var a int = b+2`,
			true,
			"int",
		},
	}

	testVarGenerate(t, cases)
}
