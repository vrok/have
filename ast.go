package have

import (
	"bytes"
	"fmt"
)

const NoName = "_"

type Expr interface {
	Pos() int
}

type expr struct {
	pos int
}

type Stmt interface {
	Expr
	Label() *Object
}

type stmt struct {
	expr
	label *Object
}

type declStmt interface {
	Decls() []string
}

// Simple statements are those that can be used in the 3rd
// clause of the `for` loop.
type SimpleStmt interface {
	Stmt
}

type TopLevelStmt struct {
	Stmt

	deps          []string
	unboundTypes  map[string][]*CustomType
	unboundIdents map[string][]*Ident
}

// List of top-level symbols used within this statement.
func (s *TopLevelStmt) Deps() []string {
	return s.deps
}

func (s *TopLevelStmt) loadDeps() {
	uniqMap := make(map[string]bool)
	for name := range s.unboundTypes {
		uniqMap[name] = true
	}
	for name := range s.unboundIdents {
		uniqMap[name] = true
	}
	s.deps = make([]string, 0, len(uniqMap))
	for name := range uniqMap {
		s.deps = append(s.deps, name)
	}
}

// List of top-level symbols declared within this statement.
func (s *TopLevelStmt) Decls() []string {
	var result []string
	switch stmt := s.Stmt.(type) {
	case *VarStmt:
		stmt.Vars.eachPair(func(v *Variable, init Expr) {
			result = append(result, v.name)
		})
	case declStmt:
		// TODO: Tests are leaking, add an interface to prevent this
		result = stmt.Decls()
	}
	return result
}

type ObjectType int

const (
	OBJECT_VAR = ObjectType(iota + 1)
	OBJECT_TYPE
	OBJECT_PACKAGE
	OBJECT_LABEL
	OBJECT_GENERIC
	OBJECT_GENERIC_TYPE
)

// This serves a similar purpose to Go's types.Object
type Object interface {
	Name() string
	ObjectType() ObjectType
}

// Implements Object
type Variable struct {
	name string
	Type Type
}

func (o *Variable) Name() string           { return o.name }
func (o *Variable) ObjectType() ObjectType { return OBJECT_VAR }

// implements Object and Stmt
type LabelStmt struct {
	stmt
	name       string
	Branchable Stmt // Either nil or branchable statement (for, switch, etc)
}

func (l *LabelStmt) Name() string           { return l.name }
func (l *LabelStmt) ObjectType() ObjectType { return OBJECT_LABEL }

type ImportStmt struct {
	stmt
	name, path string
	pkg        *Package
}

func (i *ImportStmt) Name() string           { return i.name }
func (i *ImportStmt) ObjectType() ObjectType { return OBJECT_PACKAGE }

// Implements Object and Stmt
type TypeDecl struct {
	stmt

	name        string
	AliasedType Type
	Methods     map[string]*FuncDecl
}

func (o *TypeDecl) Name() string           { return o.name }
func (o *TypeDecl) ObjectType() ObjectType { return OBJECT_TYPE }
func (o *TypeDecl) Type() Type {
	// TODO: cache this thing, don't produce new instances every time
	if o.AliasedType == nil {
		return &SimpleType{simpleTypeStrToID[o.name]}
	}
	return &CustomType{Name: o.name, Decl: o}
}

// Represents an argument to a generic statment (in early stages of compilation they
// can't be treated as type declarations - they are just placeholders for future types).
// Implements Object and Stmt
type GenericTypeDecl struct {
	stmt
	name string
}

func (g *GenericTypeDecl) Name() string           { return g.name }
func (g *GenericTypeDecl) ObjectType() ObjectType { return OBJECT_GENERIC_TYPE }

var builtinTypeNames []string = []string{"bool", "byte", "complex128", "complex64", "error", "float32",
	"float64", "int", "int16", "int32", "int64", "int8", "rune",
	"string", "uint", "uint16", "uint32", "uint64", "uint8", "uintptr"}

var builtinTypes map[string]*TypeDecl = map[string]*TypeDecl{}

func initVarDecls() {
	for _, name := range builtinTypeNames {
		builtinTypes[name] = &TypeDecl{
			name: name,
			//Type: &SimpleType{ID: simpleTypeStrToID[name]},
			AliasedType: nil, // Simple types aren't aliases
		}
	}
}

var builtinFuncs = []string{
	// These should be changed once we have varags etc.
	"func print(s interface:\n\t) bool:\n pass",
	"func read() string:\n pass",
}

func GetBuiltinType(name string) (*TypeDecl, bool) {
	t, ok := builtinTypes[name]
	return t, ok
}

type CodeBlock struct {
	Statements []Stmt
	Labels     map[string]*LabelStmt
}

func (cb *CodeBlock) AddLabel(l *LabelStmt) error {
	if _, ok := cb.Labels[l.Name()]; ok {
		return fmt.Errorf("Label `%s` declared more than once", l.Name())
	}
	cb.Labels[l.Name()] = l
	return nil
}

// implements SimpleStmt
type AssignStmt struct {
	stmt
	Lhs, Rhs []Expr
	Token    *Token
}

// implements Stmt
type SendStmt struct {
	stmt
	Lhs, Rhs Expr
}

// implements Stmt
type StructStmt struct {
	stmt
	Struct *StructType
}

// implements Stmt
type IfaceStmt struct {
	stmt
	Iface *IfaceType
}

type VarDecl struct {
	Vars  []*Variable
	Inits []Expr
}

// Calls callback for every variable-initializer pair of a VarDecl.
// When len(Vars) > len(Inits), nil value is used as trailing Inits.
func (vd *VarDecl) eachPair(callback func(v *Variable, init Expr)) {
	for i, v := range vd.Vars {
		init := Expr(nil)
		if i < len(vd.Inits) {
			init = vd.Inits[i]
		}
		callback(v, init)
	}
}

// implements Stmt
type VarStmt struct {
	stmt
	Vars       DeclChain
	IsFuncStmt bool
}

// Chain of variable declarations. Sample uses:
// 	- multi-element variable declaration
// 	- function arguments definition
//  - function results definition
type DeclChain []*VarDecl

// Calls VarDecl.eachPair for each VarDecl in the ArgsDecl.
func (ad DeclChain) eachPair(callback func(v *Variable, init Expr)) {
	for _, vd := range ad {
		vd.eachPair(callback)
	}
}

func (ad DeclChain) countVars() int {
	count := 0
	for _, vd := range ad {
		count += len(vd.Vars)
	}
	return count
}

// implements Stmt
type PassStmt struct {
	stmt
}

// implements Stmt
type IfBranch struct {
	stmt
	ScopedVarDecl *VarStmt
	Condition     Expr
	Code          *CodeBlock
}

// implements Stmt
type IfStmt struct {
	stmt
	Branches []*IfBranch
}

// implements Stmt
type SwitchBranch struct {
	stmt

	// This can carry different kinds of expression depending on the switch variant:
	//   - usually it can be any expressions comparable to the switch expression
	//   - for free-form switches this has to be one expression assignable to bool
	//   - for type switches this is a TypeExpr
	//   - `nil` for `default`
	Values []Expr
	Code   *CodeBlock
	// Type switches can declare a variable that has different type in each switch
	// branch. To keep things independent and invariant (and make the generator code less
	// fragile), we actually declare a separate variable internally for each branch
	// (all with the same name).
	// For non-type-switches this is nil.
	TypeSwitchVar *Variable
}

// implements Stmt
type SwitchStmt struct {
	stmt

	// Either AssignStmt or VarStmt
	ScopedVar Stmt
	// For free-form switch statements it is nil, for type switches it is either
	// `a.(someType)` or `var b = a.(someType)`, and for the rest it is ExprStmt.
	Value    Stmt
	Branches []*SwitchBranch
}

// implements Stmt
type ForStmt struct {
	stmt

	// TODO: foreach-like loops can't be handled by this
	ScopedVarDecl *VarStmt
	Condition     Expr
	RepeatStmt    SimpleStmt
	Code          *CodeBlock
}

// implements Stmt
// Statement wrapper for expressions.
type ExprStmt struct {
	stmt
	Expression Expr
}

// Break, continue, goto
type BranchStmt struct {
	stmt
	Token *Token
	Right *Ident

	// Is nil for breaks/continues in unnamed loops/switches.
	GotoLabel *LabelStmt
	// Is nil for goto statements. Otherwise it is the statement
	// that the continue/break refers to.
	Branchable Stmt
}

type ReturnStmt struct {
	stmt

	Func   *FuncDecl
	Values []Expr
}

type Generic interface {
	Name() string
	Generate(params ...Type) (Object, []error)
}

// Implements Stmt, Object
type GenericFunc struct {
	stmt
	Params []string
	Func   *FuncDecl
}

func (gf *GenericFunc) Name() string           { return gf.Func.name }
func (gf *GenericFunc) ObjectType() ObjectType { return OBJECT_GENERIC }
func (gf *GenericFunc) Generate(params ...Type) (Object, []error) {
	panic("TODO")
}

type GenericStruct struct {
}

func (gf *GenericFunc) ImplicitGenerate(resultType Type, paramTypes ...Type) (*FuncDecl, []error) {
	panic("TODO")
}

// Implements Type
type GenericType struct {
	Name     string
	Concrete Type
}

func (t *GenericType) Known() bool       { return t.Concrete.Known() }
func (t *GenericType) String() string    { return t.Concrete.String() }
func (t *GenericType) Kind() Kind        { return t.Concrete.Kind() }
func (t *GenericType) ZeroValue() string { return t.Concrete.ZeroValue() }

type Kind int

const (
	KIND_SIMPLE = Kind(iota + 1)
	KIND_ARRAY
	KIND_SLICE
	KIND_MAP
	KIND_POINTER
	KIND_CUSTOM
	KIND_STRUCT
	KIND_INTERFACE
	KIND_TUPLE
	KIND_FUNC
	KIND_CHAN
	KIND_UNKNOWN
)

type Type interface {
	// True means no underscores beneath, no type inference needed.
	Known() bool
	String() string
	Kind() Kind
	ZeroValue() string
}

type SimpleTypeID int

const (
	SIMPLE_TYPE_BOOL = SimpleTypeID(iota + 1)
	SIMPLE_TYPE_BYTE
	SIMPLE_TYPE_COMPLEX128
	SIMPLE_TYPE_COMPLEX64
	SIMPLE_TYPE_ERROR
	SIMPLE_TYPE_FLOAT32
	SIMPLE_TYPE_FLOAT64
	SIMPLE_TYPE_INT
	SIMPLE_TYPE_INT16
	SIMPLE_TYPE_INT32
	SIMPLE_TYPE_INT64
	SIMPLE_TYPE_INT8
	SIMPLE_TYPE_RUNE
	SIMPLE_TYPE_STRING
	SIMPLE_TYPE_UINT
	SIMPLE_TYPE_UINT16
	SIMPLE_TYPE_UINT32
	SIMPLE_TYPE_UINT64
	SIMPLE_TYPE_UINT8
	SIMPLE_TYPE_UINTPTR
)

var simpleTypeAsStr = map[SimpleTypeID]string{
	SIMPLE_TYPE_BOOL:       "bool",
	SIMPLE_TYPE_BYTE:       "byte",
	SIMPLE_TYPE_COMPLEX128: "complex128",
	SIMPLE_TYPE_COMPLEX64:  "complex64",
	SIMPLE_TYPE_ERROR:      "error",
	SIMPLE_TYPE_FLOAT32:    "float32",
	SIMPLE_TYPE_FLOAT64:    "float64",
	SIMPLE_TYPE_INT:        "int",
	SIMPLE_TYPE_INT16:      "int16",
	SIMPLE_TYPE_INT32:      "int32",
	SIMPLE_TYPE_INT64:      "int64",
	SIMPLE_TYPE_INT8:       "int8",
	SIMPLE_TYPE_RUNE:       "rune",
	SIMPLE_TYPE_STRING:     "string",
	SIMPLE_TYPE_UINT:       "uint",
	SIMPLE_TYPE_UINT16:     "uint16",
	SIMPLE_TYPE_UINT32:     "uint32",
	SIMPLE_TYPE_UINT64:     "uint64",
	SIMPLE_TYPE_UINT8:      "uint8",
	SIMPLE_TYPE_UINTPTR:    "uintptr",
}

var simpleTypeStrToID = map[string]SimpleTypeID{}

func initSimpleTypeIDs() {
	for k, v := range simpleTypeAsStr {
		simpleTypeStrToID[v] = k
	}
}

type SimpleType struct {
	ID SimpleTypeID
}

func (t *SimpleType) Known() bool    { return true }
func (t *SimpleType) String() string { return simpleTypeAsStr[t.ID] }
func (t *SimpleType) Kind() Kind     { return KIND_SIMPLE }
func (t *SimpleType) ZeroValue() string {
	switch t.ID {
	case SIMPLE_TYPE_STRING:
		return `""`
	case SIMPLE_TYPE_BOOL:
		return "false"
	default:
		return "0"
	}
}

func IsBoolAssignable(t Type) bool {
	return IsAssignable(&SimpleType{SIMPLE_TYPE_BOOL}, t)
}
func IsTypeBool(t Type) bool {
	return t.Kind() == KIND_SIMPLE && t.(*SimpleType).ID == SIMPLE_TYPE_BOOL
}
func IsTypeInt(t Type) bool {
	return t.Kind() == KIND_SIMPLE && t.(*SimpleType).ID == SIMPLE_TYPE_INT
}
func IsTypeString(t Type) bool {
	return t.Kind() == KIND_SIMPLE && t.(*SimpleType).ID == SIMPLE_TYPE_STRING
}
func IsTypeSimple(t Type, simpleType SimpleTypeID) bool {
	return t.Kind() == KIND_SIMPLE && t.(*SimpleType).ID == simpleType
}
func IsTypeIntKind(t Type) bool {
	if t.Kind() != KIND_SIMPLE {
		return false
	}
	switch t.(*SimpleType).ID {
	case SIMPLE_TYPE_INT, SIMPLE_TYPE_INT8, SIMPLE_TYPE_INT16, SIMPLE_TYPE_INT32, SIMPLE_TYPE_INT64,
		SIMPLE_TYPE_UINT8, SIMPLE_TYPE_UINT16, SIMPLE_TYPE_UINT32, SIMPLE_TYPE_UINT64, SIMPLE_TYPE_UINT,
		SIMPLE_TYPE_BYTE:
		return true
	}
	return false
}
func IsTypeFloatKind(t Type) bool {
	if t.Kind() != KIND_SIMPLE {
		return false
	}
	switch t.(*SimpleType).ID {
	case SIMPLE_TYPE_FLOAT32, SIMPLE_TYPE_FLOAT64:
		return true
	}
	return false
}
func IsTypeComplexType(t Type) bool {
	if t.Kind() != KIND_SIMPLE {
		return false
	}
	switch t.(*SimpleType).ID {
	case SIMPLE_TYPE_COMPLEX64, SIMPLE_TYPE_COMPLEX128:
		return true
	}
	return false
}
func IsTypeNumeric(t Type) bool {
	return IsTypeIntKind(t) || IsTypeFloatKind(t) || IsTypeComplexType(t) || IsTypeSimple(t, SIMPLE_TYPE_RUNE)
}

type ArrayType struct {
	Size int
	Of   Type
}

func (t *ArrayType) Known() bool    { return t.Of.Known() }
func (t *ArrayType) String() string { return fmt.Sprintf("[%d]%s", t.Size, t.Of.String()) }
func (t *ArrayType) Kind() Kind     { return KIND_ARRAY }
func (t *ArrayType) ZeroValue() string {
	b := bytes.Buffer{}
	b.WriteString(fmt.Sprintf("%s{", t))
	for i := 0; i < t.Size; i++ {
		b.WriteString(t.Of.ZeroValue())
		if i+1 < t.Size {
			b.WriteString(", ")
		}
	}
	b.WriteString("}")
	return b.String()
}

type SliceType struct {
	Of Type
}

func (t *SliceType) Known() bool       { return t.Of.Known() }
func (t *SliceType) String() string    { return "[]" + t.Of.String() }
func (t *SliceType) Kind() Kind        { return KIND_SLICE }
func (t *SliceType) ZeroValue() string { return "nil" }

type MapType struct {
	By, Of Type
}

func (t *MapType) Known() bool       { return t.By.Known() && t.Of.Known() }
func (t *MapType) String() string    { return "map[" + t.By.String() + "]" + t.Of.String() }
func (t *MapType) Kind() Kind        { return KIND_MAP }
func (t *MapType) ZeroValue() string { return "nil" }

type FuncType struct {
	Args, Results []Type
}

func (t *FuncType) Known() bool {
	for _, a := range t.Args {
		if !a.Known() {
			return false
		}
	}
	for _, r := range t.Results {
		if !r.Known() {
			return false
		}
	}
	return true
}

func (t *FuncType) String() string {
	return "func" + t.Header()
}

func (t *FuncType) Header() string {
	out := &bytes.Buffer{}
	out.WriteString("(")
	for c, a := range t.Args {
		out.WriteString(a.String())
		if (c + 1) < len(t.Args) {
			out.WriteString(", ")
		}
	}
	out.WriteString(")")
	switch len(t.Results) {
	case 0:
	case 1:
		out.WriteString(" " + t.Results[0].String())
	default:
		out.WriteString(" (")
		for c, r := range t.Results {
			out.WriteString(r.String())
			if (c + 1) < len(t.Results) {
				out.WriteString(", ")
			}
		}
		out.WriteString(")")
	}
	return out.String()
}

func (t *FuncType) Kind() Kind        { return KIND_FUNC }
func (t *FuncType) ZeroValue() string { return "nil" }

type ChanDir int

const (
	CHAN_DIR_BI = ChanDir(iota)
	CHAN_DIR_RECEIVE
	CHAN_DIR_SEND
)

type ChanType struct {
	Of  Type
	Dir ChanDir
}

func (t *ChanType) Known() bool { return t.Of.Known() }
func (t *ChanType) String() string {
	switch t.Dir {
	case CHAN_DIR_RECEIVE:
		return fmt.Sprintf("<-chan %s", t.Of)
	case CHAN_DIR_SEND:
		return fmt.Sprintf("chan<- %s", t.Of)
	default:
		return fmt.Sprintf("chan %s", t.Of)
	}
}
func (t *ChanType) Kind() Kind        { return KIND_CHAN }
func (t *ChanType) ZeroValue() string { return "nil" }

type PointerType struct {
	To Type
}

func (t *PointerType) Known() bool       { return t.To.Known() }
func (t *PointerType) String() string    { return "*" + t.To.String() }
func (t *PointerType) Kind() Kind        { return KIND_POINTER }
func (t *PointerType) ZeroValue() string { return "nil" }

type TupleType struct {
	Members []Type
}

func (t *TupleType) Known() bool {
	for _, t := range t.Members {
		if !t.Known() {
			return false
		}
	}
	return true
}

func (t *TupleType) String() string {
	out := &bytes.Buffer{}
	out.WriteByte('(')
	for c, v := range t.Members {
		fmt.Fprintf(out, v.String())
		if c+1 < len(t.Members) {
			out.Write([]byte(", "))
		}
	}
	out.WriteByte(')')
	return out.String()
}

func (t *TupleType) Kind() Kind { return KIND_TUPLE }

func (t *TupleType) ZeroValue() string { panic("this should not happen") }

type StructType struct {
	Members map[string]Type
	// Keys in the order of declaration
	Keys    []string
	Methods map[string]*FuncDecl
	Name    string
}

func (t *StructType) GetTypeN(n int) Type {
	return t.Members[t.Keys[n]]
}

func (t *StructType) Known() bool {
	for _, t := range t.Members {
		if !t.Known() {
			return false
		}
	}
	return true
}

func (t *StructType) String() string {
	out := &bytes.Buffer{}
	out.WriteString("struct {")
	for i, k := range t.Keys {
		if _, ok := t.Members[k]; !ok {
			// Not a plain member, but a method
			continue
		}
		fmt.Fprintf(out, "%s %s", k, t.Members[k].String())
		if (i + 1) < len(t.Members) {
			out.Write([]byte("; "))
		}
	}
	out.WriteByte('}')
	return out.String()
}

func (t *StructType) Kind() Kind        { return KIND_STRUCT }
func (t *StructType) ZeroValue() string { return fmt.Sprintf("%s{}", t) }

type IfaceType struct {
	// Keys in the order of declaration
	Keys    []string
	Methods map[string]*FuncDecl
	name    string
}

func (t *IfaceType) Known() bool { return true }
func (t *IfaceType) Kind() Kind  { return KIND_INTERFACE }

func (t *IfaceType) String() string {
	out := &bytes.Buffer{}
	out.WriteString("interface{")
	// TODO: use 't.Keys' for consistent order
	for i, k := range t.Keys {
		fmt.Fprintf(out, "%s%s", t.Methods[k].name, t.Methods[k].typ.Header())
		if (i + 1) < len(t.Methods) {
			out.Write([]byte("; "))
		}
	}
	out.WriteByte('}')
	return out.String()
}

func (t *IfaceType) ZeroValue() string { return "nil" }

type CustomType struct {
	// Base name of the type. Doesn't include package name for external types.
	Name string
	// nil means local
	Package *ImportStmt
	Decl    *TypeDecl
}

func (t *CustomType) Known() bool { return true }
func (t *CustomType) String() string {
	if t.Package == nil {
		return t.Name
	} else {
		return t.Package.name + "." + t.Name
	}
}
func (t *CustomType) Kind() Kind { return KIND_CUSTOM }
func (t *CustomType) RootType() Type {
	current := t.Decl.AliasedType
	for current.Kind() == KIND_CUSTOM {
		current = current.(*CustomType).Decl.AliasedType
	}
	return current
}

func (t *CustomType) ZeroValue() string { return t.RootType().ZeroValue() }

type UnknownType struct {
}

func (t *UnknownType) Known() bool       { return false }
func (t *UnknownType) String() string    { return "_" }
func (t *UnknownType) Kind() Kind        { return KIND_UNKNOWN }
func (t *UnknownType) ZeroValue() string { return "nil" }

type TypeExpr struct {
	expr
	typ Type
}

func (e *expr) Pos() int {
	return e.pos
}

func (s *stmt) Label() *Object {
	return s.label
}

// Blank expression, represents no expression.
// Sometimes useful.
// implements Expr
type BlankExpr struct {
	expr
}

func NewBlankExpr() *BlankExpr { return &BlankExpr{expr{0}} }

type NilExpr struct {
	expr
	typ Type
}

// implements Expr
type BasicLit struct {
	expr
	typ Type

	token *Token
}

type CompoundLitKind int

const (
	COMPOUND_UNKNOWN CompoundLitKind = iota
	COMPOUND_EMPTY
	COMPOUND_LISTLIKE
	COMPOUND_MAPLIKE
)

// implements Expr
type CompoundLit struct {
	expr
	Left       Expr
	typ        Type
	kind       CompoundLitKind
	elems      []Expr
	contentPos int
}

func (cl *CompoundLit) updatePosWithType(typ Expr) {
	cl.contentPos = cl.pos
	cl.pos = typ.Pos()
}

// Iterate over every key-value pair.
// It only makes sense for COMPOUND_MAPLIKE.
func (cl *CompoundLit) eachKeyVal(callback func(key, val Expr)) {
	for i := 0; i < len(cl.elems)/2; i++ {
		callback(cl.elems[i*2], cl.elems[i*2+1])
	}
}

// implements Expr
type BinaryOp struct {
	expr

	Left, Right Expr
	op          *Token
}

// implements Expr
type UnaryOp struct {
	expr

	Right Expr
	op    *Token
	// Type can be e.g. a tuple (X, bool) after type negotiation.
	typ Type
}

type PrimaryExpr interface {
	Expr
}

// implements PrimaryExpr
type ArrayExpr struct {
	expr

	Left  Expr
	Index []Expr

	// Type can be e.g. a tuple (X, bool) after type negotiation.
	typ Type
}

// Represents subslice extraction - for x[a:b], it represents a:b.
// Implements Expr.
type SliceExpr struct {
	expr

	From, To Expr
}

// implements Expr
type DotSelector struct {
	expr

	Left  Expr
	Right *Ident
}

// implements Expr
type TypeAssertion struct {
	expr

	// When true, no concrete type was given in the parentheses, just
	// the "type" keyword.
	ForSwitch bool
	Left      Expr
	Right     *TypeExpr

	// Type can be e.g. a tuple (X, bool) after type negotiation.
	typ Type
}

// implements PrimaryExpr
type FuncCallExpr struct {
	expr

	Left Expr
	//args []*Expr
	Args []Expr
}

// implements PrimaryExpr
type FuncDecl struct {
	expr

	name          string
	typ           *FuncType
	Args, Results DeclChain
	Code          *CodeBlock
	// Is nil for non-method functions
	Receiver *Variable
	// Indicates whether the receiver is a pointer.
	// It is redundant for structs, but is useful for interfaces
	// (where Receiver is nil).
	PtrReceiver bool
	// Names of generic type paramaters. Nil for standard functions.
	GenericParams []string
}

// implements PrimaryExpr
type Ident struct {
	expr

	name   string
	object Object
	// After type checking, some Ident instances turn out to be just
	// member names used in a composite literal. They aren't matched
	// with any object and it's not an error.
	memberName bool
	//token *Token
}

type Node interface {
	//Pos() int // TODO
	//End() int // TODO
}

func init() {
	initSimpleTypeIDs()
	initVarDecls()
}
