package have

import (
	"flag"
	"fmt"
	"os"
	"strings"
	"testing"

	gotoken "go/token"

	"github.com/davecgh/go-spew/spew"
)

func testPkg(t *testing.T, shouldFail bool, files []struct {
	name, file, gocode string
}) {
	var list []*File
	ctx := NewTypesContext()
	for _, input := range files {
		f := NewFile(input.name, input.file)
		f.tc = ctx
		list = append(list, f)
	}

	pkg := NewPackage("main", list...)

	errs := pkg.ParseAndCheck()

	if len(errs) > 0 {
		if shouldFail {
			return
		} else {
			t.Fail()
			fmt.Printf("Errors: %s\n", spew.Sdump(errs))
			return
		}
	}

	for i, f := range files {
		output := list[i].GenerateCode()
		if strings.TrimSpace(output) != strings.TrimSpace(f.gocode) {
			t.Fail()
			fmt.Printf("Wrong output, wanted:\n%s\nGot:\n%s\n", f.gocode, output)
		}
	}
}

func TestCompilePackageSimple(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
func main() { pass }`,
			`package main

func main() {
	// pass
}`,
		},
	}
	testPkg(t, false, files)
}

func TestCompilePackage_MemberName(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
func main() {
	struct A {
		foo int
	}
	var x = A{foo: 7} # We're not sure if 'foo' is an ident until typechecker
}`,
			`package main

func main() {
	type A struct {
		foo int
	}

	var x = (A)(A{
		foo: 7,
	})
}`,
		},
	}
	testPkg(t, false, files)
}
func TestCompilePackage_UnmatchedMemberName(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
func main() {
	struct A {
		foo int
	}
	var x = A{foob: 7} # Error, A doesn't have a member named 'foob'
}`,
			"",
		},
	}
	testPkg(t, true, files)
}

func TestCompilePackage_UnmatchedIdent(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
func main() { var y = bla }`,
			``,
		},
	}
	testPkg(t, true, files)
}

func TestCompilePackage_Loop(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
var a = b
var b = a
`,
			`package main

func main() {
	// pass
}`,
		},
	}
	testPkg(t, true, files)
}

func TestCompilePackageUnorderedBinding(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
func main() {
	var x = y
}
var y = 10`,
			`
package main

func main() {
	var x = (int)(y)
}
var y = (int)(10)
`,
		},
	}
	testPkg(t, false, files)
}

func TestCompilePackageDependentFiles(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
func main() { var x = y }`,
			`
package main

func main() {
	var x = (int)(y)
}`},
		{"world.hav",
			`package main
var y = 10`,
			`
package main

var y = (int)(10)`},
	}
	testPkg(t, false, files)
}

func TestCompilePackageGenericFunc(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
func bla[T](a T) T {
	return a
}
func main() {
	bla(7)
	bla("ble")
}
`,
			`
package main

// Generic instantiation
func bla_int(a int) (int) {
	return a
}

// Generic instantiation
func bla_string(a string) (string) {
	return a
}

func main() {
	bla_int(7)
	bla_string("ble")
}`},
	}
	testPkg(t, false, files)
}

func TestCompilePackageGenericStruct(t *testing.T) {
	files := []struct {
		name, file, gocode string
	}{
		{
			"hello.hav",
			`package main
struct bla[T] {
	t T
	func meh(a T) T {
		return a + self.t
	}
}
func main() {
	var x bla[int], y bla[string]
	x.meh(7)
	y.meh("ble")
}
`,
			`package main

// Generic instantiation
type bla_int struct {
	t int
}

func (self bla_int) meh(a int) (int) {
	return (a + self.t)
}

// Generic instantiation
type bla_string struct {
	t string
}

func (self bla_string) meh(a string) (string) {
	return (a + self.t)
}

func main() {
	var x = (bla_int)(struct {t int}{})
	var y = (bla_string)(struct {t string}{})
	x.meh(7)
	y.meh("ble")
}`},
	}
	testPkg(t, false, files)
}

type testStmt struct {
	name  string
	decls []string
}

func (t testStmt) Pos() gotoken.Pos { return 0 }
func (t testStmt) Label() *Object   { return nil }
func (t testStmt) Decls() []string  { return t.decls }

func TestStmtsSort(t *testing.T) {
	type node struct {
		name        string
		decls, deps []string
	}

	var cases = []struct {
		src        []node
		goals      [][]string
		shouldFail bool
	}{
		{
			src: []node{
				{name: "1",
					decls: []string{"a"},
					deps:  []string{"b"},
				},
				{name: "2",
					decls: []string{"b"},
					deps:  []string{"c"},
				},
				{name: "3",
					decls: []string{"c"},
					deps:  []string{},
				},
			},
			goals:      [][]string{{"3", "2", "1"}},
			shouldFail: false,
		},
		{
			src: []node{
				{name: "1",
					decls: []string{"a"},
					deps:  []string{},
				},
				{name: "2",
					decls: []string{"b"},
					deps:  []string{"a"},
				},
				{name: "3",
					decls: []string{"c"},
					deps:  []string{"b"},
				},
			},
			goals:      [][]string{{"1", "2", "3"}},
			shouldFail: false,
		},
		{
			src: []node{
				{name: "1",
					decls: []string{"a"},
					deps:  []string{},
				},
				{name: "2",
					decls: []string{"b"},
					deps:  []string{"a"},
				},
				{name: "3",
					decls: []string{"c"},
					deps:  []string{"a"},
				},
			},
			goals:      [][]string{{"1", "2", "3"}, {"1", "3", "2"}},
			shouldFail: false,
		},
		{
			src: []node{
				{name: "1",
					decls: []string{"a", "aa"},
					deps:  []string{},
				},
				{name: "2",
					decls: []string{"b"},
					deps:  []string{"a"},
				},
				{name: "3",
					decls: []string{"c"},
					deps:  []string{"aa"},
				},
			},
			goals:      [][]string{{"1", "2", "3"}, {"1", "3", "2"}},
			shouldFail: false,
		},
		{
			src: []node{
				{name: "1",
					decls: []string{"a"},
					deps:  []string{},
				},
				{name: "2",
					decls: []string{"b", "c"},
					deps:  []string{"a"},
				},
				{name: "3",
					decls: []string{"d"},
					deps:  []string{"b"},
				},
			},
			goals:      [][]string{{"1", "2", "3"}},
			shouldFail: false,
		},
		{
			src: []node{
				{name: "1",
					decls: []string{"a"},
					deps:  []string{"b"},
				},
				{name: "2",
					decls: []string{"b"},
					deps:  []string{"c"},
				},
				{name: "3",
					decls: []string{"c"},
					deps:  []string{"a"},
				},
			},
			goals:      nil,
			shouldFail: true,
		},
		{
			src: []node{
				{name: "1",
					decls: []string{"a", "aa"},
					deps:  []string{"b"},
				},
				{name: "2",
					decls: []string{"b"},
					deps:  []string{"a"},
				},
				{name: "3",
					decls: []string{"c"},
					deps:  []string{"aa"},
				},
			},
			goals:      nil,
			shouldFail: true,
		},
	}

	for i, c := range cases {
		if *justCase >= 0 && i != *justCase {
			continue
		}
		input := []*TopLevelStmt{}
		for _, node := range c.src {
			stmt := testStmt{name: node.name}
			stmt.decls = node.decls

			tls := &TopLevelStmt{Stmt: stmt, unboundIdents: map[string][]*Ident{}}
			for _, dep := range node.deps {
				tls.unboundIdents[dep] = nil
			}
			tls.loadDeps()

			input = append(input, tls)
		}

		l, err := topoSort(input)
		if c.shouldFail {
			if err == nil {
				t.Fail()
				fmt.Printf("Case should have failed")
			}
		} else {
			if len(l) != len(input) {
				t.Fail()
				fmt.Printf("Different length: %d and %d", len(l), len(input))
			}
			ok := false
		goalsLoop:
			for _, goal := range c.goals {
				for i := range goal {
					//fmt.Printf("%s -- %s\n", goal[i], l[i].Stmt.(testStmt).name)
					if goal[i] != l[i].Stmt.(testStmt).name {
						//fmt.Printf("Difference on pos %d\n", i)
						continue goalsLoop
					}
				}
				ok = true
				break
			}

			if !ok {
				fmt.Printf("Wrong order, not found in the possible orders (case %d)", i)
				t.Fail()
			}
		}
	}
}

// Implements PkgLocator
type fakeLocator struct {
	files map[string][]*File
}

type fakeLocatorFile struct {
	pkg, name, code string
}

func newFakeLocator(files ...fakeLocatorFile) *fakeLocator {
	result := fakeLocator{make(map[string][]*File, len(files))}
	ctx := NewTypesContext()
	for _, file := range files {
		f := NewFile(file.name, file.code)
		f.tc = ctx
		result.files[file.pkg] = append(result.files[file.pkg], f)
	}
	return &result
}

func (l *fakeLocator) Locate(pkgPath string) ([]*File, error) {
	files, ok := l.files[pkgPath]
	if !ok {
		return nil, fmt.Errorf("Package %s can't be found", pkgPath)
	}
	return files, nil
}

func testPkgImport(t *testing.T, files []fakeLocatorFile, outputRef map[string]string, shouldFail bool) {
	locator := newFakeLocator(files...)
	manager := NewPkgManager(locator)

	pkg, errs := manager.Load("a")

	if shouldFail {
		if len(errs) == 0 {
			t.Fail()
			fmt.Println("Error: Should've failed but didn't")
		}
		return
	} else {
		if len(errs) > 0 {
			t.Fail()
			fmt.Printf("Error: %s\n", spew.Sdump(errs))
			return
		}
	}

	for i, f := range pkg.Files {
		if f.Name == BuiltinsFileName {
			continue
		}
		output := f.GenerateCode()
		if strings.TrimSpace(output) != strings.TrimSpace(outputRef[f.Name]) {
			t.Fail()
			fmt.Printf("ERROR, different output code for case #%d\n", i)
			fmt.Printf("-- Source:\n%s\n-- Wanted:\n%s\n-- Got:\n%s\n", f.Code, outputRef[f.Name], output)
		}
	}
}

func TestPkgImport(t *testing.T) {
	files := []fakeLocatorFile{
		{"a", "a.hav", `package a
import "b"
var aaa = 123 + b.bbb`},
		{"b", "b.hav", `package b
var bbb float32 = 321`},
	}

	outputCode := map[string]string{
		"a.hav": `package a

import b "b"
var aaa = (float32)((123 + b.bbb))`,
	}

	testPkgImport(t, files, outputCode, false)
}

func TestPkgImport_Type(t *testing.T) {
	files := []fakeLocatorFile{
		{"a", "a.hav", `package a
import "b"
var aaa b.B = 123`},
		{"b", "b.hav", `package b
type B int`},
	}

	outputCode := map[string]string{
		"a.hav": `package a

import b "b"
var aaa = (b.B)(123)`,
	}

	testPkgImport(t, files, outputCode, false)
}

func TestPkgImport_TypeCasts(t *testing.T) {
	// Type casts/conversions deserve a separate test because they are handled
	// a bit differently - they look like function calls before type checking.
	files := []fakeLocatorFile{
		{"a", "a.hav", `package a
import "b"
var aaa = b.B(123)`},
		{"b", "b.hav", `package b
type B int`},
	}

	outputCode := map[string]string{
		"a.hav": `package a

import b "b"
var aaa = (b.B)(b.B(123))`,
	}

	testPkgImport(t, files, outputCode, false)
}

func TestPkgImport3_Line(t *testing.T) {
	files := []fakeLocatorFile{
		{"a", "a.hav", `package a
import "b"
var aaa = 123 + b.bbb`},
		{"b", "b.hav", `package b
import "c"
var bbb = 321 + c.ccc`},
		{"c", "c.hav", `package c
var ccc float32 = 456`},
	}

	outputCode := map[string]string{
		"a.hav": `package a

import b "b"
var aaa = (float32)((123 + b.bbb))`,
	}

	testPkgImport(t, files, outputCode, false)
}

func TestPkgImport3_OpenJaw(t *testing.T) {
	files := []fakeLocatorFile{
		{"a", "a.hav", `package a
import "b"
import "c"
var aaa = b.bbb + c.ccc`},
		{"b", "b.hav", `package b
var bbb float32 = 123`},
		{"c", "c.hav", `package c
var ccc float32 = 456`},
	}

	outputCode := map[string]string{
		"a.hav": `package a

import b "b"
import c "c"
var aaa = (float32)((b.bbb + c.ccc))`,
	}

	testPkgImport(t, files, outputCode, false)
}

func TestPkgImport_Cycle(t *testing.T) {
	files := []fakeLocatorFile{
		{"a", "a.hav", `package a
import "b"
var aaa = 123 + b.bbb`},
		{"b", "b.hav", `package b
import "a"
var bbb float32 = 321`},
	}

	outputCode := map[string]string{
		"a.hav": `package a

import b "b"
var aaa = (float32)((123 + b.bbb))`,
	}

	testPkgImport(t, files, outputCode, true)
}

var justCase = flag.Int("case", -1, "Run only selected test case")

func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
