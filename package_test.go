package have

import (
	"flag"
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/davecgh/go-spew/spew"
)

func testPkg(t *testing.T, shouldFail bool, files []struct {
	name, file, gocode string
}) {
	var list []*File
	for _, input := range files {
		f := NewFile(input.name, input.file, nil)
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
func main():
	pass`,
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
func main():
	struct A:
		foo int
	var x = A{foo: 7} # We're not sure if 'foo' is an ident until typechecker`,
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
func main():
	struct A:
		foo int
	var x = A{foob: 7} # Error, A doesn't have a member named 'foob'`,
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
func main():
	var y = bla`,
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
func main():
	var x = y
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
func main():
	var x = y`,
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

type testStmt struct {
	name  string
	decls []string
}

func (t testStmt) Pos() int        { return 0 }
func (t testStmt) Label() *Object  { return nil }
func (t testStmt) Decls() []string { return t.decls }

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

var justCase = flag.Int("case", -1, "Run only selected test case")

func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
