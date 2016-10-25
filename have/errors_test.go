package have

import (
	"fmt"
	"testing"
)

func testErrors(t *testing.T, files []fakeLocatorFile, errsTarget []string) {
	locator := newFakeLocator(files...)
	manager := NewPkgManager(locator)

	_, errs := manager.Load("a")

	if len(errs) != len(errsTarget) {
		t.Fail()
		fmt.Printf("Wrong number of errors, want %d, got %d\n", len(errsTarget), len(errs))
		return
	}

	for i := 0; i < len(errs); i++ {
		if compErr, ok := errs[i].(*CompileError); ok {
			if compErr.PrettyString(manager.Fset) != errsTarget[i] {
				t.Fail()
				fmt.Printf("Wrong compile error, want: \n\t'%s'\n, got \n\t'%s'\n", errsTarget[i], compErr.PrettyString(manager.Fset))
			}
		} else {
			if errs[i].Error() != errsTarget[i] {
				t.Fail()
				fmt.Printf("Wrong error, want: \n\t'%s'\n, got \n\t'%s'\n", errsTarget[i], errs[i])
			}
		}
	}
}

func TestErrors(t *testing.T) {
	var cases = []struct {
		files  []fakeLocatorFile
		errors []string
	}{
		{
			[]fakeLocatorFile{fakeLocatorFile{"a", "a.hav", `package a
func main() {
	var x int = "aaa"
}`}}, []string{"a.hav:3: Can't use this literal for type int"},
		},

		{
			[]fakeLocatorFile{fakeLocatorFile{"a", "a.hav", `package a
~`}}, []string{"a.hav:2: Unexpected token (expected a primary expression): TOKEN_UNEXP_CHAR"},
		},

		{
			[]fakeLocatorFile{fakeLocatorFile{"a", "a.hav", `package a
func main() {
	somethingUnknown()
}`}}, []string{"a.hav:3: Unknown identifier: somethingUnknown"},
		},

		{
			[]fakeLocatorFile{fakeLocatorFile{"a", "a.hav", `package a
func main() {
	somethingUnknown[int]()
}`}}, []string{"a.hav:3: Unknown identifier: somethingUnknown"},
		},
	}

	for _, c := range cases {
		testErrors(t, c.files, c.errors)
	}
}
