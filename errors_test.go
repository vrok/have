package have

import (
	"fmt"
	"testing"
)

func testErrors(t *testing.T, files []fakeLocatorFile, errsTarget []error) {
	locator := newFakeLocator(files...)
	manager := NewPkgManager(locator)

	_, errs := manager.Load("a")

	if len(errs) != len(errsTarget) {
		t.Fail()
		fmt.Print("Wrong number of errors, want %d, got %d\n", len(errsTarget), len(errs))
		return
	}

	for i := 0; i < len(errs); i++ {
		if errs[i].Error() != errsTarget[i].Error() {
			t.Fail()
			fmt.Printf("Wrong error, want: \n\t'%s'\n, got \n\t'%s'\n", errsTarget[i], errs[i])
		}
	}
}

func TestErrors_Simple(t *testing.T) {
	files := []fakeLocatorFile{
		{"a", "a.hav", `package a
func main():
	var x int = "aaa"
`},
	}

	testErrors(t, files, []error{fmt.Errorf("Can't use this literal for type int")})
}
