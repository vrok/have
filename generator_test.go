package have

import (
	"fmt"
	"io/ioutil"
	"testing"
)

func transpile(code string, outputFile string) error {
	parser := NewParser(NewLexer([]rune(code)))
	result, err := parser.ParseFile()
	if err != nil {
		return err
	}

	for _, stmt := range result.Statements {
		typedStmt := stmt.(ExprToProcess)
		if err := typedStmt.NegotiateTypes(); err != nil {
			return err
		}
	}

	cc := &CodeChunk{}
	result.Generate(cc)

	return ioutil.WriteFile(outputFile, []byte(cc.ReadAll()), 0644)
}

func TestGenerate(t *testing.T) {
	cases := []string{
		"helloworld",
	}

	for i, c := range cases {
		code, err := ioutil.ReadFile(fmt.Sprintf("samples/%s.hav", c))
		if err != nil {
			panic(err)
		}

		fmt.Printf("Compiling case %d: %s", i, transpile(string(code), fmt.Sprintf("tmp/case_%d.go", i)))
	}
}
