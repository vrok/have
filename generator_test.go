package have

import (
	"fmt"
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
		if result != c.reference {
			t.Fail()
			fmt.Printf("Different input and output for case %d.\nInput: %s\nOutput: %s\nWanted: %s\n",
				i, c.source, result, c.reference)
		}
	}
}

func TestGenerateExpr(t *testing.T) {
	cases := []generatorTestCase{
		{source: "1", reference: "(int)(1)\n"},
		{source: "\"bla\"", reference: "(string)(\"bla\")\n"},
		{source: "true", reference: "true\n"},
		{source: "false", reference: "false\n"},
	}
	testCases(t, cases)
}
