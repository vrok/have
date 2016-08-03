package have

import (
	"fmt"
	"strings"
	"testing"
)

type typeTestCase struct {
	code       string
	shouldPass bool
	typ        string
}

// Helper for tests. Takes a scrap of code, parses & typechecks it as if it
// was a package.
func processFileAsPkg(code string) (*Package, []*TopLevelStmt, []error) {
	f := NewFile("main.go", "package main\n"+code, nil, nil)

	pkg := NewPackage("main", f)
	errs := pkg.ParseAndCheck()

	return pkg, pkg.files[0].statements, errs
}

func testVarTypes(t *testing.T, cases []typeTestCase) {
	for i, c := range cases {
		if *justCase >= 0 && i != *justCase {
			continue
		}

		pkg, stmts, errs := processFileAsPkg(strings.TrimSpace(c.code))

		var err error
		if len(errs) > 0 {
			err = errs[0]
		}

		if (err == nil) != c.shouldPass {
			t.Fail()
			fmt.Printf("FAIL: Case %d: Bad code accepted or good code parsed with an error for '%s'\nError: %s\n",
				i, c.code, err)
			return
		}

		if c.shouldPass {
			firstDecl := stmts[len(stmts)-1].Stmt.(*VarStmt).Vars[0]

			firstVar, firstInit := firstDecl.Vars[0], Expr(nil)
			if len(firstDecl.Inits) > 0 {
				firstInit = firstDecl.Inits[0]
			}

			fit, err := firstInit.(TypedExpr).Type(pkg.tc)
			if firstVar.Type.String() != c.typ || !IsAssignable(fit, firstVar.Type) || err != nil {
				t.Fail()
				fmt.Printf("FAIL: Case %d: Bad type: %s, %s, %s\n", i, c.typ, firstVar.Type.String(),
					fit.String())
			}
		}
	}
}

func TestWithLookups(t *testing.T) {
	var cases = []typeTestCase{
		{`var b int = 2		
var a int = b`,
			true,
			"int",
		},
		{`var b = 2
var a int = b`,
			true,
			"int",
		},
		{`var b = 2
var a int = b * 2`,
			true,
			"int",
		},
		{`var b int = 2, c int = 30, d int = 40
var a int = b * c + d + 10`,
			true,
			"int",
		},
		{`var b = 2, c = 30, d = 40
var a int = b * c + d + 10`,
			true,
			"int",
		},
		{`type number int
var a number = 1`,
			true,
			"number",
		},
		{`type text string
var a text = "trele morele"`,
			true,
			"text",
		},
		{`type number int
var a number = "trele morele"`,
			false,
			"",
		},
		{`type text string
type scribble text
var a scribble= "trele morele"`,
			true,
			"scribble",
		},
		{`type pint *int
var x = 1
var a pint = &x`,
			true,
			"pint",
		},
		{`type text string
var x string = "1"
var a text = x`,
			false,
			"",
		},
		{`type hash map[int]string
var b hash = {1: "aaa"}
var a hash = b`,
			true,
			"hash",
		},
		{`type hash map[int]string
var b hash = {1: "aaa"}
var a map[int]string = b`,
			true,
			"map[int]string",
		},
		{`type hash map[int]string
var b map[int]string = {1: "aaa"}
var a hash = b`,
			true,
			"hash",
		},
		{`type chain []string
var b []string = {"aaa"}
var a chain = b`,
			true,
			"chain",
		},
		{`type hash map[int]string
var b hash = {1: "aaa"}
var a map[string]string = b`,
			false,
			"",
		},
		{`var b, c, d = 2, 30, 40
var a int = b * c + d + 10`,
			true,
			"int",
		},
		{`var b = struct:
	x int
var y int = b.x`,
			true,
			"int",
		},
		{`var b = struct:
	x int
var y = b.x`,
			true,
			"int",
		},
		{`var b = struct:
	c struct:
		d string
var f = b.c.d`,
			true,
			"string",
		},
		{`var b = ((*struct:
	c string)(&{c: "ech"}))
var f = b.c`,
			true,
			"string",
		},
		{`var b = ((int)(1))`,
			true,
			"int",
		},
		{`var b = struct:
	x int
var y string = b.x`,
			false,
			"",
		},
		{`var b int = 2		
var a string = b`,
			false,
			"",
		},
		{`var b = 2		
var a string = b`,
			false,
			"",
		},
		{`func f() int:
	var x = 1
var a int = f()`,
			true,
			"int",
		},
		{`func f() int:
	var x int = "a"
var a = f()`,
			false,
			"",
		},
		{`func f() string:
	var x = 1
var a int = f()`,
			false,
			"",
		},
		{`func f():
	var x = 1
var a int = f()`,
			false,
			"",
		},
		{`func f(x int) int:
	var x = 1
var a int = f(4)`,
			true,
			"int",
		},
		{`func f(x string) int:
	var x = 1
var a int = f(4)`,
			false,
			"",
		},
		{`func f(x string, y int) int:
	var x = 1
var b int = 5
var a int = f("las", b)`,
			true,
			"int",
		},
		{`func f(x string, y int) int:
	var x = 1
var b string = "5"
var a int = f("las", b)`,
			false,
			"",
		},
		{`func f() int:
	if true:
		var y = 2
	var x = 1
var a int = f()`,
			true,
			"int",
		},
		{`func f() int:
	if 1:
		var y = 2
	var x = 1
var a int = f()`,
			false,
			"",
		},
		{`var x = true || false`,
			true,
			"bool",
		},
		{`var x = 1 || 2`,
			false,
			"",
		},
		{`func f(x int) int:
	for x = 0; x < 100; print("a"):
		var y = 2
var a int = f(100)`,
			true,
			"int",
		},
		{`func f() int:
	for var x = 0; x < 100; print("a"):
		var y = 2
var a int = f()`,
			true,
			"int",
		},
		{`func x(s string):
	pass
func f() int:
	for var x = 0; x < 100; x(1):
		pass
var a int = f()`,
			false,
			"int",
		},
		{`func f() int:
	for ;;:
		pass
var a int = f()`,
			true,
			"int",
		},
		{`func f():
	var x = 1
	x = 2`,
			true,
			"func()",
		},
		{`func f():
	var x = 1
	x = "bla"`,
			false,
			"",
		},
	}

	testVarTypes(t, cases)
}

func TestTypesForRange(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
for var x range {1, 2, 3}:
	var a int = x
var placeholder = 1`,
			true,
			"int",
		},
		{`
for var x, y range {"1", "2", "3"}:
	var a int = x, b string = y
var placeholder = 1`,
			true,
			"int",
		},
		{`
for var x, y, y range {1, 2, 3}: # Too many vars
	pass
var placeholder = 1`,
			false,
			"",
		},
		{`
for var x range {1, 2, 3}:
	var a int = "a" # Fail to make sure the code block is typechecked
var placeholder = 1`,
			false,
			"",
		},
		{`
for var x, y range []string{"1", "2", "3"}:
	var a int = x, b string = y
var placeholder = 1`,
			true,
			"int",
		},
		{`
for var x, y range [3]string{"1", "2", "3"}:
	var a int = x, b string = y
var placeholder = 1`,
			true,
			"int",
		},
		{`
for var x, y range map[float32]string{1: "1", 2: "2", 3: "3"}:
	var a float32 = x, b string = y
var placeholder = 1`,
			true,
			"int",
		},
		{`
for var x, y range map[float32]string{1: "1", 2: "2", 3: "3"}:
	var a float32 = x, b int = y # int and string aren't assignable
var placeholder = 1`,
			false,
			"",
		},
		{`
var x float32, y string
for x, y range map[float32]string{1: "1", 2: "2", 3: "3"}:
	var a float32 = x, b string = y
var placeholder = x`,
			true,
			"float32",
		},
		{`
var x float32, y string
for x, z range map[float32]string{1: "1", 2: "2", 3: "3"}: # z is unknown
	pass
var placeholder = x`,
			false,
			"",
		},
	})
}

func TestCustomStructTypes(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`type point struct:
	x int
	y int
var a point = {}
var b point = a`,
			true,
			"point",
		},
		{`type point struct:
	x int
	y int
var a point = {1, 2}
var b point = a`,
			true,
			"point",
		},
		{`type point struct:
	x int
	y int
var a = point{1, 2}`,
			true,
			"point",
		},
		{`type point struct:
	x int
	y int
var a point = {x: 1, y: 2}`,
			true,
			"point",
		},
		{`type point struct:
	x int
var a = point{}
var b = a.x`,
			true,
			"int",
		},
		{`type point struct:
	x int
	y int
var a point = {x: 1, z: 2}`,
			false,
			"",
		},
		{`type point struct:
	x int
	y int
var a point = {x: 1, y: "2"}`,
			false,
			"",
		},
		{`type point struct:
	x int
	y int
var a struct:
	x int
	y int = {}
var b point = a`,
			true,
			"point",
		},
	})
}

func TestTypesIfStmt(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`func f() int:
	if 1 == 2:
		var y = 2
	var x = 1
var a int = f()`,
			true,
			"int",
		},
	})
}

func TestTypesTupleAssign(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
func a() (int, int):
	pass
var x, y = a()
var z = x`,
			true,
			"int",
		},
		{`
func a() (int, int):
	pass
var x, y int = a()
var z = x`,
			true,
			"int",
		},
		{`
func a() (int, string):
	pass
var x, y = a()
var z = y`,
			true,
			"string",
		},
		{`
func a() (int, string):
	pass
var x, y = a()
var z int = y`,
			false,
			"",
		},
		{`
func a() (int, int):
	pass
var x, y int
x, y = a()
var z = x`,
			true,
			"int",
		},
		{`
func a() (int, int):
	pass
func b(x, y int) int:
	pass
var z = b(a())`,
			true,
			"int",
		},
		{`
func a() (int, string):
	pass
func b(x, y int) int:
	pass
var z = b(a())`,
			false,
			"",
		},
		{`
type B []int
func a() (int, []int):
	return 1, {}
var x int, y B
x, y = a()
var z = y`,
			true,
			"B",
		},
	})
}

func TestTypesStruct(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
struct Abc:
	func x():
		pass
var a = Abc{}`,
			true,
			"Abc",
		},
		{`
struct Abc:
	x int
	func x():
		pass
var a = Abc{x: 7}`,
			true,
			"Abc",
		},
		{`
struct Abc:
	x int
	func x():
		pass
var a = Abc{y: 7}`,
			false,
			"Abc",
		},
		{`
struct Abc:
	func x(z int):
		z = 3
		pass
var a = Abc{}`,
			true,
			"Abc",
		},
		{`
struct Abc:
	zz int
	func x(z int):
		self.zz = z
	func *xp(z int):
		self.zz = z
		(*self).zz = z
var a = Abc{}`,
			true,
			"Abc",
		},
		{`
struct Abc:
	func x() int:
		pass
var a Abc
var b = a.x()
`,
			true,
			"int",
		},
		{`
struct Abc:
	func x() int:
		pass
var b = Abc{}.x()
`,
			true,
			"int",
		},
		{`
struct Abc:
	func x() (int, string):
		pass
var b, c = Abc{}.x()
var d = c
`,
			true,
			"string",
		},
	})
}

func TestTypesInterfaces(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
interface A:
	func x()
struct Abc:
	func x():
		pass
var a A
a = Abc{}
var b = a
`,
			true,
			"A",
		},
		{`
interface A:
	func x()
struct Abc:
	func *x():
		pass
var a A
a = &Abc{}
var b = a
`,
			true,
			"A",
		},
		{`
interface A:
	func x()
struct Abc:
	func *x():
		pass
var a A
var b *Abc = &Abc{}
var c A = 7
`,
			false,
			"",
		},
		{`
interface A:
	func x()
struct Abc:
	func *x():
		pass
var b *Abc = &Abc{}
var c A = b
var d = "placeholder for current test framework - remove this line to see why"
`,
			true,
			"string",
		},
		// TODO: Below case doesn't compile as it should, but the error msg is very non-intuitive:
		{`
interface A:
	func x()
struct Abc:
	func y():
		pass
var a A
a = Abc{}
var b = a
`,
			false,
			"",
		},
		{`
interface A:
	func x()
struct Abc:
	func x():
		pass
func zab() (Abc, int):
	pass
func ka(a A, i int) A:
	pass	
var c = ka(zab())
`,
			true,
			"A",
		},
		{`
interface A:
	func x()
struct Abc:
	func y():
		pass
func zab() (Abc, int):
	pass
func ka(a A, i int) A:
	pass	
var c = ka(zab())
`,
			false,
			"",
		},
		{`
interface A:
	func x()
struct Abc:
	func x():
		pass
func z() (Abc, int):
	pass
var a A, b int
a, b = z()
var c = a
`,
			true,
			"A",
		},
		{`
interface A:
	func x()
struct Abc:
	func x():
		pass
func z() (Abc, int):
	pass
var a A, b int
a, b = z()
var c = a
`,
			true,
			"A",
		},
		{`
struct Abc:
	func x():
		pass
var a interface:
	func x()
a = Abc{}
var b = a
`,
			true,
			"interface{x()}",
		},
		{`
func p(value interface:
	) int:
	pass
var x = p("aaa")`,
			true,
			"int"},
	})
}

func TestTypesInterfaceMethods(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
interface A:
       func x() int
var a A
var b = a.x()
`,
			true,
			"int",
		},
	})
}

func TestTypesSendExpr(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
var x chan int
x <- 1
var y = x`,
			true,
			"chan int",
		},
		{`
var x chan int
x <- "bla"
var y = x`,
			false,
			"",
		},
		{`
var x chan<- int
x <- 5
var y = x`,
			true,
			"chan<- int",
		},
		{`
var x <-chan int
x <- 5
var y = x`,
			false,
			"",
		},
	})
}

func TestTypesReturnStmt(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
func a() int:
	return 7
var x = a()
`,
			true,
			"int",
		},
		{`
func a() string:
	return 7
var x = a()
`,
			false,
			"",
		},
		{`
func a() (int, int):
	return 1
var x = a()
`,
			false,
			"",
		},
		{`
struct A:
	x int
func a() *A:
	return &{1}
var x = a()
`,
			true,
			"*A",
		},
	})
}

func TestTypesTmp(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
type B []int
func a() (int, []int):
	return 1, {}
var x int, y B
x, y = a()
var z = y`,
			true,
			"B",
		},
	})
}

func TestTypesTypeAssertion(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
interface A:
	func x()
struct B:
	func x():
		pass
var x A
var y = x.(B)`,
			true,
			"B",
		},
		{`
interface A:
	func x()
struct B:
	func xx():
		pass
var x A
var y = x.(B)`,
			false, // B doesn't implement A
			"",
		},
		{`
struct B:
	func x():
		pass
var x B
var y = x.(B)`,
			false, // non-interface on left
			"",
		},
		{`
interface A:
	func x()
struct B:
	func x():
		pass
var x A
var y, z = x.(B)
var final = z`,
			true,
			"bool",
		},
	})
}

func TestTypesSwitch(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
var a = 7
switch a
case 1:
	pass
default:
	pass
var c = true
`,
			true,
			"bool",
		},
		{`
var a = 7
switch a
case "bla":
	pass
var c = true
`,
			false,
			"",
		},
		{`
var a = 7
switch a
case 1, 2, 3:
	pass
var c = true
`,
			true,
			"bool",
		},
		{`
var a = 7
switch a
case 1, 4.5, 3:
	pass
var c = true
`,
			false,
			"",
		},
		{`
switch
case 1 == 2:
	pass
var c = true
`,
			true,
			"bool",
		},
		{`
switch
case 1 == 2, true:
	pass
var c = true
`,
			false,
			"",
		},
		{`
switch
case "bla":
	pass
var c = true
`,
			false,
			"",
		},
		{`
switch
case true:
	var c int = "not_an_int"
var c = true
`,
			false,
			"",
		},
		{`
switch var x = 7; x
case 5:
	x = 8
var c = true
`,
			true,
			"bool",
		},
		{`
var x int
switch x = 7; x
case 5:
	x = 8
var c = true
`,
			true,
			"bool",
		},
	})
}

func TestTypesTypeSwitch(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
var bla interface:
	pass
switch bla.(type)
case int:
	pass
var y = true`, true, "bool"},
		{`
var bla interface:
	func a()
struct x:
	func a():
		pass
switch bla.(type)
case x: # Error: impossible assertion, x doesn't implement the interface
	pass
var y = true`, true, "bool"},
		{`
var bla interface:
	func a()
struct x:
	pass
switch bla.(type)
case x: # Error: impossible assertion, x doesn't implement the interface
	pass
var y = true`, false, ""},
		{`
var bla int
switch bla.(type) # Error: non-interface used for type switch
case int:
	pass
var y = true`, false, ""},
		{`
var bla interface:
	pass
switch var x = bla.(type)
case int:
	pass
var y = true`, true, "bool"},
		{`
var bla interface:
	pass
switch var x = bla.(int) # "int" instead of "type"
case int:
	pass
var y = true`, false, ""},
		{`
var bla interface:
	pass
switch var x = bla.(type)
case "ble": # Error: not a type name
	pass
var y = true`, false, ""},
		{`
var bla interface:
	pass
switch var x = bla.(type)
case int:
	var z int = x
var y = true`, true, "bool"},
		{`
var bla interface:
	pass
switch var x = bla.(type)
case int:
	var z string = x # Error: string and int are not assignable
var y = true`, false, ""},
		{`
var bla = 123
var bla interface:
	pass
switch var x = bla.(type)
case int:
	var z int = x
case string:
	var z string = x
var y = true`, true, "bool"},
	})
}

func TestTypesRecvExpr(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
var a chan int
var b = <-a`,
			true,
			"int",
		},
		{`
var a <-chan int
var b = <-a`,
			true,
			"int",
		},
		{`
var a chan<- int
var b = <-a`,
			false,
			"",
		},
		{`
var a string
var b = <-a`,
			false,
			"",
		},
		{`
type Ch chan int
var a Ch
var b = <-a`,
			true,
			"int",
		},
		{`
var a chan int
var b, c = <-a
var d = c`,
			true,
			"bool",
		},
		{`
var a chan int
var b, c = <-a
var d = b`,
			true,
			"int",
		},
	})
}

func TestTypesRunes(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
var a rune = 'a'
`,
			true,
			"rune",
		},
		{`
var a rune = 100
`,
			true,
			"rune",
		},
		{`
var a = 'a'
`,
			true,
			"rune",
		},
	})
}

func TestTypesBlankIdent(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
var _ = 1
`,
			true,
			"int",
		},
		{`
var _ = 1
_ = 2
var _ = 3
`,
			true,
			"int",
		},
		{`
var _ = 1
var a int = _
`,
			false,
			"",
		},
		{`
func a() (int, string):
	return 1, "a"
var _, _ = a()
var placeholder = 1
`,
			true,
			"int",
		},
		{`
func a() (int, string):
	return 1, "a"
var s string
_, s = a()
var placeholder = 1
`,
			true,
			"int",
		},
		{`
var a int
for _, a range {1, 2, 3}:
	pass
var placeholder = 1
`,
			true,
			"int",
		},
		{`
var a string
for _, a range {1, 2, 3}:
	pass
var placeholder = 1
`,
			false,
			"",
		},
		{`
if true: # check in a block
	_ = 1
var placeholder = 1
`,
			true,
			"int",
		},
		{`
func a() (int, string):
	return 1, "a"
if var _, s = a(); s == "a":
	pass
var placeholder = 1
`,
			true,
			"int",
		},
		{`
func a() (int, string):
	return 1, "a"
var s string
if _, s = a(); s == "a":
	pass
var placeholder = 1
`,
			true,
			"int",
		},
	})
}

func TestTypesNumberLiterals(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`var b = 1e10`,
			true,
			"float64",
		},
		{`var b = 0.5`,
			true,
			"float64",
		},
		{`var b float32 = 0.5`,
			true,
			"float32",
		},
		{`var b = 50`,
			true,
			"int",
		},
		{`var b float32 = 50`,
			true,
			"float32",
		},
		{`var b = 50i`,
			true,
			"complex128",
		},
	})
}

func TestTypesNil(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`var a *int = nil`,
			true,
			"*int",
		},
		{`
interface A:
	func x()
var a A = nil`,
			true,
			"A",
		},
		{`
interface A:
	func x()
var a = A(nil)`,
			true,
			"A",
		},
		{`
var a interface:
	func x()
  = nil`,
			true,
			"interface{x()}",
		},
		{`var a int = nil`,
			false,
			"",
		},
		{`var a string = nil`,
			false,
			"",
		},
		{`
struct A:
	x int
var a A = nil`,
			false,
			"",
		},
		{`var a *int = nil
var b = a == nil`,
			true,
			"bool",
		},
	})
}

func TestTypesCompareLiterals(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`var x []int
var y = x == {1, 2, 3}`,
			true,
			"bool",
		},
		{`var x []int
var y = x == {1, "bla", 3}`,
			false,
			"",
		},
		{`var x []int
var y = x == nil`,
			true,
			"bool",
		},
		{`var x []int
var y = nil == x`,
			true,
			"bool",
		},
		{`var y = nil == nil`,
			false,
			"",
		},
		{`var x map[string]int
var y = x == {"a": 1, "b": 2, "c": 3}`,
			true,
			"bool",
		},
		{`var x map[string]int
var y = x == {1, 2, 3}`,
			false,
			"",
		},
	})
}

func TestTypesIndices(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`var x []int
var y = x[0]`,
			true,
			"int",
		},
		{`var x [7]int
var y = x[0]`,
			true,
			"int",
		},
		{`var x [7]int
var y = 5 == x[0]`,
			true,
			"bool",
		},
		{`var y = 5 == {1,2,3,4}[0]`,
			true,
			"bool",
		},
		{`var y = "lala"[0]`,
			true,
			"byte",
		},
		{`var x = &([3]int{0,1,2})
var y = x[0]`,
			true,
			"int",
		},
		{`var x = &([]int{0,1,2})
var y = x[0]`,
			false,
			"",
		},
		{`var x map[string]int
var y = x["bla"]`,
			true,
			"int",
		},
		{`var x map[string]int
var y = x[5]`,
			false,
			"",
		},
		{`var x []int
var y, z = x[0]
var a = y`,
			false,
			"",
		},
		{`var x map[string]int
var y, z = x["a"]
var a = y`,
			true,
			"int",
		},
		{`var x map[string]int
var y, z = x["a"]
var a = z`,
			true,
			"bool",
		},
		{`var x map[string]float32
var y int, z bool
y, z = x["a"]
var a = y`,
			false,
			"",
		},
		{`var x map[string]int
func f(a int, b bool) int:
	pass
var a = f(x["b"])`,
			false,
			"",
		},
		{`var x map[string]int
var y int, z bool 
y, z = x["a"]
var a = y`,
			true,
			"int",
		},
		{`var x map[string]int
var y int, z bool
y, z = x["a"]
var a = z`,
			true,
			"bool",
		},
		{`var x map[string]int
var y, z int
y, z = x["a"]
var a = z`,
			false,
			"",
		},
		{`var x []int
var y = x[1:5]`,
			true,
			"[]int",
		},
		{`var x [7]int
var y = x[1:5]`,
			true,
			"[]int",
		},
		{`var x string
var y = x[1:5]`,
			true,
			"[]byte",
		},
		{`var x *[7]int
var y = x[1:5]`,
			true,
			"[]int",
		},
		{`var x *[]int
var y = x[1:5]`,
			false,
			"",
		},
		{`var x int
var y = x[1:5]`,
			false,
			"",
		},
	})
}

func TestTypesGenericFunc(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
func a[T]() int: # Something very simple for start
	return 1
var x = a[float32]()`,
			true,
			"int",
		},
		{`
func a[T]() T:
	return 1
var x = a[float32]()`,
			true,
			"float32",
		},
		{`
func a[T](x T) T:
	return 1 + x
var x = a[float32](4)`,
			true,
			"float32",
		},
		{`
func a[T](x T) T: # Trying to add string literal "aaa" to float32
	return "aaa" + x
var x = a[float32](4)`,
			false,
			"",
		},
		{`
func a[T, K](x T, y K) T:
	return x + y
var x = a[float32, float32](4, 5)`,
			true,
			"float32",
		},
		{`
func a[T, K](x T, y K) T:
	return x + y # Error, can't add float32 and string
var x = a[float32, string](4, "s")`,
			false,
			"",
		},
		{`
func a[T](x T) T: # a[T] used in a[T]
	return x + a[T](10)
var x = a[float32](4)`,
			true,
			"float32",
		},
		{`
func a[T](x T) T:
	return x
var x = a[float32]`,
			true,
			"func(float32) float32",
		},
		{`
func a[T](x T) T:
	return x
var x func(float32)float32 = a[float32]`,
			true,
			"func(float32) float32",
		},
		{`
func a[T](x T) T:
	return x
var x = a(1.2)`,
			true,
			"float64",
		},
	})
}

func TestTypesGenericTypes(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
struct A[T]:
	func x() T:
		return 1
var a A[int]
var x = a.x()`,
			true,
			"int",
		},
		{`
struct A[T]:
	func x() T:
		return "a"
struct B[T]:
	func y() T:
		var a A[T]
		return a.x()
var b B[string]
var x = b.y()`,
			true,
			"string",
		},
		{`
struct A[T]:
	func x() T:
		return "a"
struct B[T]:
	func y(a A[T]) T:
		return a.x()
var a A[string], b B[string]
var x = b.y(a)`,
			true,
			"string",
		},
		{`
struct A[T]:
	func x() T:
		return 11.2
func x[T](a A[T]) T:
	return a.x()
var a A[float32]
var x = x(a)`,
			true,
			"float32",
		},
		{`
struct A[T]:
	func x() T:
		return 11
interface I:
	func x() float32
var a A[float32]
var i I = a
var x = i`,
			true,
			"I",
		},
		{`
struct A[T]:
	func x() T:
		return 11
interface I:
	func x() float32
var a A[int]
var i I = a # Error: x() returns int, not float32
var x = i`,
			false,
			"I",
		},
	})
}

func TestTypesSimple(t *testing.T) {
	var cases = []struct {
		code       string
		shouldPass bool
		typ        string
	}{
		{`var a int = 1`,
			true,
			"int",
		},
		{`var a *int = &1`,
			true,
			"*int",
		},
		{`var a int = *&1`,
			true,
			"int",
		},
		{`var a = &*&1`,
			true,
			"*int",
		},
		{`var a *int = *1`,
			false,
			"",
		},
		{`var a = &1`,
			true,
			"*int",
		},
		{`var a string = "reksio"`,
			true,
			"string",
		},
		{`var a bool = true`,
			true,
			"bool",
		},
		{`var a bool = false`,
			true,
			"bool",
		},
		{`var a = true`,
			true,
			"bool",
		},
		{`var a = "blabla"`,
			true,
			"string",
		},
		{`var a = 123`,
			true,
			"int",
		},
		{`var a string = 1`,
			false,
			"", // whatever, shouldn't pass anwyay
		},
		{`var a int = "123"`,
			false,
			"", // whatever, shouldn't pass anwyay
		},
		{`var a bool = 0`,
			false,
			"", // whatever, shouldn't pass anwyay
		},
		{
			`var a []int = {1, 2, 3}`,
			true,
			"[]int",
		},
		{
			`var a [3]int = {1, 2, 3}`,
			true,
			"[3]int",
		},
		{
			`var a [2]int = {1, 2, 3}`,
			false,
			"",
		},
		{
			`var a []int = {1, "2", 3}`,
			false,
			"",
		},
		{
			`var a []int = {}`,
			true,
			"[]int",
		},
		{
			`var a [][]int = {{1}, {1,2}, {}}`,
			true,
			"[][]int",
		},
		{
			`var a [][]int = {{1}, {1,2}, {"1"}}`,
			false,
			"",
		},
		{
			`var a = {1, 2, 3}`,
			true,
			"[]int",
		},
		{
			`var a = {"bla", "2", "3"}`,
			true,
			"[]string",
		},
		{
			`var a = {"bla", "2", 3}`,
			false,
			"",
		},
		{
			`var a = {{"bla", "2"}, {"3"}}`,
			true,
			"[][]string",
		},
		{
			`var a = {{"bla", "2"}, {3}}`,
			false,
			"",
		},
		{
			`var a map[int]string = {1: "a", 2: "b"}`,
			true,
			"map[int]string",
		},
		{
			`var a = {1: "a", 2: "b"}`,
			true,
			"map[int]string",
		},
		{
			`var a = {1: {"a"}, 2: {"b", "c"}}`,
			true,
			"map[int][]string",
		},
		{
			`var a map[int]string = {1: "a", "2": "b"}`,
			false,
			"",
		},
		{
			`var a map[int]string = {1: "a", 2: 3}`,
			false,
			"",
		},
		{
			`var a int = 2 + 2`,
			true,
			"int",
		},
		{
			`var a = 2 + 2`,
			true,
			"int",
		},
		{
			`var a int = 2 + {1}`,
			false,
			"",
		},
		{
			`var a = 2 + {1}`,
			false,
			"",
		},
		{
			`var a int = +2`,
			true,
			"int",
		},
		{
			`var a = +2`,
			true,
			"int",
		},
	}

	for i, c := range cases {
		if *justCase >= 0 && i != *justCase {
			continue
		}
		parser := NewParser(NewLexer([]rune(c.code)))
		result, err := parser.parseVarStmt(true)
		if err != nil {
			t.Fail()
			fmt.Printf("Case %d: Failed parsing: %s\n", i, err)
		}
		ctx := NewTypesContext()
		err = result.Vars[0].NegotiateTypes(ctx)

		if (err == nil) != c.shouldPass {
			t.Fail()
			fmt.Printf("Case %d: Bad code accepted or good code parsed with an error for '%s'\nError: %s\n",
				i, c.code, err)
			continue
		}

		if err == nil {
			firstDecl := result.Vars[0]

			firstVar, firstInit := firstDecl.Vars[0], Expr(nil)
			if len(firstDecl.Inits) > 0 {
				firstInit = firstDecl.Inits[0]
			}

			fit, err := firstInit.(TypedExpr).Type(ctx)
			if firstVar.Type.String() != c.typ || fit.String() != c.typ || err != nil {
				t.Fail()
				fmt.Printf("Case %d: Bad type: %s, %s, %s\n", i, c.typ, firstVar.Type.String(),
					fit.String())
				fmt.Printf("Code:\n%s\n", c.code)
			}
		}
	}
}

func TestGenericFuncDeduction(t *testing.T) {
	cases := []struct {
		code, want, err string
	}{
		{`
var x int
func f[T](arg T) T:
	pass
f(x)`,
			"f[int]",
			"",
		},
		{`
func f[T](arg T) T:
	pass
f(1)`,
			"f[int]",
			"",
		},
		{`
var x int
func f[T](a1, a2 T) T:
	pass
f(x, 1)`,
			"f[int]",
			"",
		},
		{`
var x float32
func f[T](a1, a2 T) T:
	pass
f(x, 1)`,
			"f[float32]",
			"",
		},
		{`
var x float32
func f[T](a1, a2 T) T:
	pass
f(x, "aaa")`,
			"f[float32]",
			"Can't use this literal for type float32",
		},
		{`
var x float32
func f[T](a1, a2 T) T:
	pass
f("aaa", "a")`,
			"f[string]",
			"",
		},
		{`
var x int
func f[T](arg *T) T:
	pass
f(&x)`,
			"f[int]",
			"",
		},
		{`
var x map[string]float32
func f[T, K](arg map[T]K) T:
	pass
f(x)`,
			"f[string, float32]",
			"",
		},
		{`
var x []float32
func f[T](arg []T) T:
	pass
f(x)`,
			"f[float32]",
			"",
		},
		{`
var x map[*int][]float32
func f[T, K](arg map[T]K) T:
	pass
f(x)`,
			"f[*int, []float32]",
			"",
		},
		{`
var x map[*int][]float32
func f[T, K](arg map[*T][]K) T:
	pass
f(x)`,
			"f[int, float32]",
			"",
		},
		{`
var x func(int)int
func f[T](arg func(T)T) T:
	pass
f(x)`,
			"f[int]",
			"",
		},
		{`
var x int
var y string
func f[T](a1, a2 T) T:
	pass
f(x, y)`,
			"",
			"T can't be both int and string",
		},
		{`
func f[T](a1, a2 T) T:
	pass
f(1, "aaa")`,
			"",
			"T can't be both int and string",
		},
	}

	for i, c := range cases {
		if *justCase >= 0 && i != *justCase {
			continue
		}

		pkg, _, errs := processFileAsPkg(strings.TrimSpace(c.code))

		var err error
		if len(errs) > 0 {
			err = errs[0]
		}

		tc := pkg.tc

		if c.err != "" {
			if err == nil || !strings.Contains(err.Error(), c.err) {
				t.Fatalf("Case %d: Didn't return error containing `%s`, but `%s`", i, c.err, err)
			} else {
				continue
			}
		}

		if len(tc.instantiations) != 1 {
			t.Fatalf("Case %d: Unexpected number of instantiations: %d", i, len(tc.instantiations))
		}

		for k, _ := range tc.instantiations {
			if string(k) != c.want {
				t.Fatalf("Case %d: Deduced wrong arguments: %s instead of %s", i, k, c.want)
			}
		}
	}
}

func TestTypesExprStmt(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
func f():
    pass
f()
var placeholder int = 0`,
			true,
			"int",
		},
		{`
func f[T]():
    pass
f[int]()
var placeholder int = 0`,
			true,
			"int",
		},
		{`
func f[T](a T):
    pass
f(1)
var placeholder int = 0`,
			true,
			"int",
		},
		{`
if true:
	1 + 1 # Error: expression evaluated but not used (it's only checked for blocks ATM)
var placeholder int = 0`,
			false,
			"",
		},
	})
}

/*
func TestTypesLateIdentLookup(t *testing.T) {
	testVarTypes(t, []typeTestCase{
		{`
var a A
struct A:
	pass
var b = a
`,
			true,
			"A",
		},
	})
}
*/
