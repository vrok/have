package have

import (
	"bytes"
	"fmt"
)

type Expr interface {
	Pos() int
}

type expr struct {
	pos int
}

type Stmt interface {
	Expr
}

type ObjectType int

const (
	OBJECT_VAR = ObjectType(iota + 1)
	OBJECT_TYPE
)

// This serves a similar purpose to Go's types.Object
type Object interface {
	Name() string
	ObjectType() ObjectType
}

// Implements Object
type VarDecl struct {
	name string
	Type Type
	Init Expr
}

func (o *VarDecl) Name() string           { return o.name }
func (o *VarDecl) ObjectType() ObjectType { return OBJECT_VAR }

// Implements Object
type TypeDecl struct {
	expr

	name string
	Type Type
}

func (o *TypeDecl) Name() string           { return o.name }
func (o *TypeDecl) ObjectType() ObjectType { return OBJECT_TYPE }

type CodeBlock struct {
	Statements []Stmt
}

// implements Stmt
type VarStmt struct {
	expr
	Vars []*VarDecl
}

// implements Stmt
type IfBranch struct {
	expr
	ScopedVarDecl *VarStmt
	Condition     Expr
	Code          *CodeBlock
}

// implements Stmt
type IfStmt struct {
	expr
	Branches []*IfBranch
}

// implements Stmt
// Statement wrapper for expressions.
type ExprStmt struct {
	Expression Expr
}

type Kind int

const (
	KIND_SIMPLE = Kind(iota + 1)
	KIND_ARRAY
	KIND_SLICE
	KIND_MAP
	KIND_POINTER
	KIND_CUSTOM
	KIND_STRUCT
	KIND_TUPLE
	KIND_UNKNOWN
)

type Type interface {
	// True means no underscores beneath, no type inference needed.
	Known() bool
	String() string
	Kind() Kind
	// Underlying type. See Go docs for specification.
	UnderType() Type
	Negotiate(other Type) (Type, error)
}

type SimpleTypeID int

const (
	SIMPLE_TYPE_INT = SimpleTypeID(iota + 1)
	SIMPLE_TYPE_STRING
	SIMPLE_TYPE_BOOL
	// TODO: add others
)

var simpleTypeAsStr = map[SimpleTypeID]string{
	SIMPLE_TYPE_INT:    "int",
	SIMPLE_TYPE_STRING: "string",
	SIMPLE_TYPE_BOOL:   "bool",
}

var simpleTypeStrToID = map[string]SimpleTypeID{}

func init() {
	for k, v := range simpleTypeAsStr {
		simpleTypeStrToID[v] = k
	}
}

type SimpleType struct {
	ID SimpleTypeID
}

func (t *SimpleType) Known() bool     { return true }
func (t *SimpleType) String() string  { return simpleTypeAsStr[t.ID] }
func (t *SimpleType) Kind() Kind      { return KIND_SIMPLE }
func (t *SimpleType) UnderType() Type { return t }

type ArrayType struct {
	Size int
	Of   Type
}

func (t *ArrayType) Known() bool     { return t.Of.Known() }
func (t *ArrayType) String() string  { return fmt.Sprintf("[%d]%s", t.Size, t.Of.String()) }
func (t *ArrayType) Kind() Kind      { return KIND_ARRAY }
func (t *ArrayType) UnderType() Type { return t }

type SliceType struct {
	Of Type
}

func (t *SliceType) Known() bool     { return t.Of.Known() }
func (t *SliceType) String() string  { return "[]" + t.Of.String() }
func (t *SliceType) Kind() Kind      { return KIND_SLICE }
func (t *SliceType) UnderType() Type { return t }

type MapType struct {
	By, Of Type
}

func (t *MapType) Known() bool     { return t.By.Known() && t.Of.Known() }
func (t *MapType) String() string  { return "map[" + t.By.String() + "]" + t.Of.String() }
func (t *MapType) Kind() Kind      { return KIND_MAP }
func (t *MapType) UnderType() Type { return t }

type PointerType struct {
	To Type
}

func (t *PointerType) Known() bool     { return t.To.Known() }
func (t *PointerType) String() string  { return "*" + t.To.String() }
func (t *PointerType) Kind() Kind      { return KIND_POINTER }
func (t *PointerType) UnderType() Type { return &PointerType{To: t.To.UnderType()} }

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
		if c < len(t.Members) {
			out.Write([]byte(", "))
		}
	}
	out.WriteByte(')')
	return out.String()
}

func (t *TupleType) Kind() Kind { return KIND_TUPLE }
func (t *TupleType) UnderType() Type {
	members := make([]Type, len(t.Members))
	for i, mem := range t.Members {
		members[i] = mem.UnderType()
	}
	return &TupleType{Members: members}
}

type StructType struct {
	Members map[string]Type
	// Keys in the order of declaration
	Keys []string
}

func (st *StructType) GetTypeN(n int) Type {
	return st.Members[st.Keys[n]]
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
	out.WriteByte('{')
	for i, k := range t.Keys {
		fmt.Fprintf(out, "%s: %s", k, t.Members[k].String())
		if (i + 1) < len(t.Members) {
			out.Write([]byte(", "))
		}
	}
	out.WriteByte('}')
	return out.String()
}

func (t *StructType) Kind() Kind { return KIND_STRUCT }

func (t *StructType) UnderType() Type {
	members := make(map[string]Type)
	for name, mem := range t.Members {
		members[name] = mem.UnderType()
	}
	return &StructType{Members: members}
}

type CustomType struct {
	Name    string
	Package string // "" means local
	Decl    *TypeDecl
}

func (t *CustomType) Known() bool    { return true }
func (t *CustomType) String() string { return t.Name }
func (t *CustomType) Kind() Kind     { return KIND_CUSTOM }
func (t *CustomType) RootType() Type {
	current := t.Decl.Type
	for current.Kind() == KIND_CUSTOM {
		current = current.(*CustomType).Decl.Type
	}
	return current
}
func (t *CustomType) UnderType() Type {
	return t.Decl.Type.UnderType()
}

type UnknownType struct {
}

func (t *UnknownType) Known() bool     { return false }
func (t *UnknownType) String() string  { return "_" }
func (t *UnknownType) Kind() Kind      { return KIND_UNKNOWN }
func (t *UnknownType) UnderType() Type { return t }

type TypeExpr struct {
	expr
	typ Type
}

func (e *expr) Pos() int {
	return e.pos
}

// Blank expression, represents no expression.
// Sometimes useful.
// implements Expr
type BlankExpr struct {
	expr
}

func NewBlankExpr() *BlankExpr { return &BlankExpr{expr{0}} }

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
	typ   Type
	kind  CompoundLitKind
	elems []Expr
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
}

// When UnaryOp's operator is `*`, it can have twofold meaning, it can:
//   - be a pointer dereference
//   - be a beginning of a type name
// This function checks if the latter is true and returns the type if so.
func (uo *UnaryOp) CheckForTypeName() (typ Type, ok bool) {
	if uo.op.Type != TOKEN_MUL {
		return nil, false
	}
	if te, ok := uo.Right.(*UnaryOp); ok {
		subType, ok := te.CheckForTypeName()
		if !ok {
			return nil, false
		}
		return &PointerType{To: subType}, true
	}
	if te, ok := uo.Right.(*TypeExpr); ok {
		return &PointerType{To: te.typ}, true
	}
	return nil, false
}

type PrimaryExpr interface {
	Expr
}

// implements PrimaryExpr
type ArrayExpr struct {
	expr

	Left, Index Expr
}

type DotSelector struct {
	expr

	Left  Expr
	Right *Ident
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

	Name          string
	Args, Results []*VarDecl
	Code          *CodeBlock
}

// implements PrimaryExpr
type Ident struct {
	expr

	name   string
	object Object
	//token *Token
}

type Node interface {
	//Pos() int // TODO
	//End() int // TODO
}
