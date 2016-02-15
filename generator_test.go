package have

import (
	"fmt"
	"io/ioutil"
	"testing"
)

func transpile(code string, outputFile string) error {
	parser := NewParser(NewLexer([]rune(code)))
	result, err := parser.Parse()
	if err != nil {
		return err
	}

	cc := &CodeChunk{}
	for _, stmt := range result {
		typedStmt := stmt.(ExprToProcess)
		if err := typedStmt.NegotiateTypes(); err != nil {
			return err
		}

		typedStmt.(Generable).Generate(cc)
	}

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
