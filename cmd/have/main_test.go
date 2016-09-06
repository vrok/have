package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"reflect"
	"testing"
)

type nothing struct{}

func currentPkg() string {
	return reflect.TypeOf(nothing{}).PkgPath()
}

func currentPkgFullPath() string {
	return path.Join(os.Getenv("GOPATH"), "src", currentPkg())
}

// Compare a directory against a model directory and return the differences as
// a list of human-readable errors. If there are differences, not all of them
// are shown, in certain cases the function stops the recursive walk when some
// differences were already encountered.
func compareDirs(src, model string) (errs []error) {
	srcList, err := ioutil.ReadDir(src)
	if err != nil {
		errs = append(errs, err)
		return
	}

	modelList, err := ioutil.ReadDir(src)
	if err != nil {
		errs = append(errs, err)
		return
	}

	set := map[string]bool{}
	for _, d := range modelList {
		set[d.Name()] = true
	}

	for _, d := range srcList {
		if !set[d.Name()] {
			errs = append(errs, fmt.Errorf("Directory %s is missing %s", src, d.Name()))
		}
	}

	if len(errs) > 0 {
		return
	}

	if len(model) < len(src) {
		errs = append(errs, fmt.Errorf("Directory %s has excessive files", src))
		return
	}

	for i := 0; i < len(srcList); i++ {
		srcFile, modelFile := path.Join(src, srcList[i].Name()), path.Join(model, srcList[i].Name())

		if srcList[i].IsDir() {
			errs = append(errs, compareDirs(srcFile, modelFile)...)
		} else {
			srcContent, err := ioutil.ReadFile(srcFile)
			if err != nil {
				errs = append(errs, err)
			}
			modelContent, err := ioutil.ReadFile(modelFile)

			if err != nil {
				errs = append(errs, err)
			}

			if string(srcContent) != string(modelContent) {
				errs = append(errs, fmt.Errorf("File %s has different content from %s", srcFile, modelFile))
			}
		}
	}
	return
}

func TestTrans(t *testing.T) {
	output, err := exec.Command("go", "build", ".").CombinedOutput()
	if err != nil {
		panic(errors.New("Can't compile 'have' command: " + string(output)))
	}

	cases := []struct {
		name string
		args []string
	}{
		{"hello_world",
			[]string{
				"hello",
			},
		},
		{"scattered_world",
			[]string{
				"hello",
			},
		},
		{"simple_nested",
			[]string{
				"hel/lo",
			},
		},
	}

	for _, c := range cases {
		cmd := exec.Command("./have", append([]string{"trans"}, c.args...)...)

		testCaseDir := path.Join(currentPkgFullPath(), "test_data", c.name)

		gopath := path.Join(testCaseDir, "output")

		os.RemoveAll(gopath)
		os.MkdirAll(gopath, 0744)

		cmd.Env = append(cmd.Env,
			"GOPATH="+gopath,
			"HAVESRCPATH="+path.Join(testCaseDir, "input"))

		output, err := cmd.CombinedOutput()
		if err != nil {
			panic(errors.New("Can't compile 'have' command: " + string(output)))
		}

		var targetDir = path.Join(testCaseDir, "target")

		errs := compareDirs(gopath, targetDir)
		if len(errs) > 0 {
			t.Fail()

			fmt.Printf("Differences for case %s:\n", c.name)
			for _, e := range errs {
				fmt.Printf("    %s\n", e)
			}
		}
	}
}
