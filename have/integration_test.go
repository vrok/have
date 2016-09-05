package have

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
	"testing"
)

func transpile(code string, outputFile string) error {
	pkg, _, errs := processFileAsPkg(strings.TrimSpace(code))
	if len(errs) > 0 {
		return errs[0]
	}
	output := pkg.Files[0].GenerateCode()
	return ioutil.WriteFile(outputFile, []byte(output), 0644)
}

func TestGenerate(t *testing.T) {
	cases := []string{
		"helloworld",
		"fizzbuzz",
		"check_builtins",
	}

	for i, c := range cases {
		code, err := ioutil.ReadFile(fmt.Sprintf("samples/%s.hav", c))
		if err != nil {
			panic(err)
		}

		os.MkdirAll("tmp", 0744)

		model, sample := fmt.Sprintf("samples/%s.go", c), fmt.Sprintf("tmp/case_%d.go", i)

		err = transpile(string(code), sample)
		if err != nil {
			fmt.Printf("Failed compilation of case %d: %s", i, err)
			t.Fail()
			return
		}

		modelOutput, err := exec.Command("go", "run", model).CombinedOutput()
		if err != nil {
			panic(err)
		}
		sampleOutput, err := exec.Command("go", "run", sample).CombinedOutput()
		if err != nil {
			panic(err)
		}

		if !bytes.Equal(modelOutput, sampleOutput) {
			fmt.Printf("Files gave different outputs: %s and %s\n", model, sample)
			t.Fail()
		}
	}
}
