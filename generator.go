package have

import (
	"bytes"
	"fmt"
	"strings"
)

// CodeChunk can either be a string of smaller CodeChunks
// or its value can be a string.
// Think of it as of a union type.
// Every statement should have a separate CodeChunk.
// Statements that (directly) contain a block of code should
// be flagged (it will be used to insert code that "spills"
// beyond the current block of code).
type CodeChunk struct {
	chunks []*CodeChunk
	code   string

	// Indicates if this CodeChunk is composed of statements
	// (it means that other statements can be inserted between them,
	// and the result will be valid code)
	blockOfStmts bool

	// If this CodeChunk is a member of `chunks` of some other CodeChunk,
	// this will point to thas other CodeChunk (and will be nil otherwise)
	parent *CodeChunk

	indent string
}

// TODO: implement it in a io.Reader form, not keeping all results in memory
func (cc *CodeChunk) ReadAll() string {
	return cc.readAll("")
}

func (cc *CodeChunk) readAll(indent string) string {
	if cc.code != "" {
		return cc.code
	}

	buf := bytes.Buffer{}
	for _, chk := range cc.chunks {
		trailer := ""
		if cc.blockOfStmts {
			trailer = "\t"
			buf.WriteString(indent + trailer)
		}
		buf.WriteString(chk.readAll(indent + trailer))
	}
	return buf.String()
}

func (cc *CodeChunk) AddString(s string) {
	cc.chunks = append(cc.chunks, cc.NewStrChunk(s))
}

func (cc *CodeChunk) AddChunks(chunks ...*CodeChunk) {
	cc.chunks = append(cc.chunks, chunks...)
}

func (cc *CodeChunk) NewStrChunk(s string) *CodeChunk {
	ch := &CodeChunk{code: s, parent: cc}
	//if cc.blockOfStmts {
	//	ch.code = cc.indent + cc.code
	//}
	return ch
}

// Created a new empty chunk whose parent is the receiver.
func (cc *CodeChunk) NewChunk() *CodeChunk {
	ch := &CodeChunk{parent: cc, indent: cc.indent}
	//if cc.blockOfStmts {
	//	ch.AddString(cc.indent)
	//}
	cc.chunks = append(cc.chunks, ch)
	return ch
}

func (cc *CodeChunk) NewBlockChunk() *CodeChunk {
	ch := cc.NewChunk()
	ch.blockOfStmts = true
	return ch
}

// Format with fmt.Sprintf, but one addition: "%C" can be use to add Generables.
func (cc *CodeChunk) AddChprintf(format string, a ...interface{}) {
	var nonGenerables []interface{}

	parts := strings.Split(format, "%C")

	i := 0
	for _, arg := range a {
		if arg == nil {
			arg = EmptyGenerable{}
		}

		switch v := arg.(type) {
		case Generable:
			//generables = append(generables, v)
			cc.AddString(fmt.Sprintf(parts[i], nonGenerables...))
			nonGenerables = nil
			i++

			v.Generate(cc)
		default:
			nonGenerables = append(nonGenerables, v)
		}
	}

	// Leftovers
	cc.AddString(fmt.Sprintf(parts[i], nonGenerables...))
}

type Generable interface {
	Generate(current *CodeChunk)
}

type EmptyGenerable struct{}

func (eg EmptyGenerable) Generate(current *CodeChunk) {}

func (id *Ident) Generate(current *CodeChunk) {
	current.AddString(id.name)
}

func (lit *BasicLit) Generate(current *CodeChunk) {
	val := ""
	switch lit.token.Type {
	case TOKEN_TRUE:
		current.AddString("true")
		return
	case TOKEN_FALSE:
		current.AddString("false")
		return
	case TOKEN_NUM:
		val = lit.token.Value.(string)
	case TOKEN_STR:
		val = fmt.Sprintf("\"%s\"", lit.token.Value.(string))
	default:
		panic("impossible")
	}

	// Don't generate type conversions if not necessary.
	current.AddString(fmt.Sprintf("(%s)(%s)", lit.typ, val))
}

func (op *UnaryOp) Generate(current *CodeChunk) {
	// TODO: Put the right operator in
	current.AddChprintf("(%s%C)", op.op.Value.(string), op.Right.(Generable))
}

func (op *BinaryOp) Generate(current *CodeChunk) {
	// TODO: Put the right operator in
	current.AddChprintf("(%C %s %C)", op.Left.(Generable), op.op.Value.(string), op.Right.(Generable))
}

func (vd *VarDecl) Generate(current *CodeChunk) {
	current = current.NewChunk()
	current.AddChprintf("%s %s", vd.name, vd.Type)
	if vd.Init != nil {
		current.AddChprintf(" = %C", vd.Init.(Generable))
	}
}

func (vs *VarStmt) Generate(current *CodeChunk) {
	if vs.IsFuncStmt {
		vs.Vars[0].Init.(Generable).Generate(current)
		return
	}
	for _, vd := range vs.Vars {
		current.AddChprintf("var %C\n", vd)
	}
}

func (vs *VarStmt) GenerateShortVarDecl(current *CodeChunk) {
	if vs == nil || len(vs.Vars) == 0 {
		return
	}

	for i, vd := range vs.Vars {
		current.AddString(vd.name)
		if i+1 < len(vs.Vars) {
			current.AddString(", ")
		}
	}
	current.AddString(" := ")
	for i, vd := range vs.Vars {
		current.AddChprintf("(%s)(%C)", vd.Type, vd.Init.(Generable))
		if i+1 < len(vs.Vars) {
			current.AddString(", ")
		}
	}
}

func (fc *FuncCallExpr) Generate(current *CodeChunk) {
	current.AddChprintf("%C(", fc.Left.(Generable))
	for i, arg := range fc.Args {
		arg.(Generable).Generate(current)
		if i+1 < len(fc.Args) {
			current.AddString(", ")
		}
	}
	current.AddString(")")
}

func (fd *FuncDecl) Generate(current *CodeChunk) {
	current = current.NewChunk()
	current.AddChprintf("func %s(", fd.name)
	for i, arg := range fd.Args {
		arg.Generate(current)
		if i+1 < len(fd.Args) {
			current.AddString(", ")
		}
	}
	current.AddString(")")

	if len(fd.Results) > 0 {
		current.AddString(" (")
		for i, arg := range fd.Results {
			arg.Generate(current)
			if i+1 < len(fd.Args) {
				current.AddString(", ")
			}
		}
		current.AddString(")")
	}

	current.AddString(" {\n")
	fd.Code.Generate(current)

	current.AddString("}\n")
}

func (bl *CodeBlock) Generate(current *CodeChunk) {
	block := current.NewBlockChunk()
	for _, stmt := range bl.Statements {
		//fmt.Printf("ZZZ generate code for %s\n", spew.Sdump(stmt))
		stmt.(Generable).Generate(block.NewChunk())
	}
}

func (es *ExprStmt) Generate(current *CodeChunk) {
	current.AddChprintf("%C\n", es.Expression.(Generable))
}

func (fs *IfStmt) Generate(current *CodeChunk) {
	current = current.NewChunk()

	if fs.Branches[0].ScopedVarDecl != nil {
		current.AddString("if ")
		fs.Branches[0].ScopedVarDecl.GenerateShortVarDecl(current)
		current.AddChprintf("; %C {\n", fs.Branches[0].Condition)
	} else {
		current.AddChprintf("if %C {\n", fs.Branches[0].Condition)
	}

	fs.Branches[0].Code.Generate(current)
	current.AddString("}")

	for i, branch := range fs.Branches {
		if i == 0 {
			continue // It's already generated
		}

		if branch.Condition != nil {
			current.AddChprintf(" else if %C {\n", branch.Condition)
		} else {
			current.AddString(" else {\n")
		}

		branch.Code.Generate(current)
		current.AddString("}")
	}

	current.AddString("\n")
}

func (fs *ForStmt) Generate(current *CodeChunk) {
	current = current.NewChunk()

	// TODO: Handle `for` variants other than 3-way
	current.AddString("for ")
	fs.ScopedVarDecl.GenerateShortVarDecl(current)
	current.AddChprintf("; %C; %C {\n%C}\n", fs.Condition, fs.RepeatExpr, fs.Code)
}

func (f *File) Generate(current *CodeChunk) {
	current.AddChprintf("package %s\n\n", f.Pkg)
	for _, stmt := range f.Statements {
		stmt.(Generable).Generate(current)
	}
}

// TODO: Now just write Generables for all statements/expressions and we're done...
