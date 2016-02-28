package have

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os/exec"
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
		"fizzbuzz",
	}

	for i, c := range cases {
		code, err := ioutil.ReadFile(fmt.Sprintf("samples/%s.hav", c))
		if err != nil {
			panic(err)
		}

		model, sample := fmt.Sprintf("samples/%s.go", c), fmt.Sprintf("tmp/case_%d.go", i)

		fmt.Printf("Compiling case %d: %s", i, transpile(string(code), sample))

		modelOutput, err := exec.Command("go", "run", model).CombinedOutput()
		if err != nil {
			panic(err)
		}
		sampleOutput, err := exec.Command("go", "run", sample).CombinedOutput()
		if err != nil {
			panic(err)
		}

		if bytes.Compare(modelOutput, sampleOutput) != 0 {
			fmt.Printf("Files gave different outputs: %s and %s\n", model, sample)
			t.Fail()
		}
	}
}
