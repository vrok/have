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

type VarDecl struct {
	Name string
	Type Type
	Init Expr
}

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

type Type interface {
	// True means no underscores beneath, no type inference needed.
	Known() bool
	String() string
}

type SimpleType struct {
	Name string
}

func (t *SimpleType) Known() bool    { return true }
func (t *SimpleType) String() string { return t.Name }

type ArrayType struct {
	Size int
	Of   Type
}

func (t *ArrayType) Known() bool    { return t.Of.Known() }
func (t *ArrayType) String() string { return fmt.Sprintf("[%d]%s", t.Size, t.Of.String()) }

type SliceType struct {
	Of Type
}

func (t *SliceType) Known() bool    { return t.Of.Known() }
func (t *SliceType) String() string { return "[]" + t.Of.String() }

type MapType struct {
	By, Of Type
}

func (t *MapType) Known() bool    { return t.By.Known() && t.Of.Known() }
func (t *MapType) String() string { return "map[" + t.By.String() + "]" + t.Of.String() }

type PointerType struct {
	To Type
}

func (t *PointerType) Known() bool    { return t.To.Known() }
func (t *PointerType) String() string { return "*" + t.To.String() }

type StructType struct {
	Members map[string]Type
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
	out.Write([]byte("{"))
	c := 0
	for k, v := range t.Members {
		fmt.Fprintf(out, "%s: %s", k, v.String())
		c++
		if c < len(t.Members) {
			out.Write([]byte(", "))
		}
	}
	out.Write([]byte("}"))
	return out.String()
}

type CustomType struct {
	Name    string
	Package string // "" means local
}

func (t *CustomType) Known() bool    { return true }
func (t *CustomType) String() string { return t.Name }

type UnknownType struct {
}

func (t *UnknownType) Known() bool    { return false }
func (t *UnknownType) String() string { return "_" }

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
type FuncCall struct {
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

	name string
	//token *Token
}

type Node interface {
	//Pos() int // TODO
	//End() int // TODO
}
