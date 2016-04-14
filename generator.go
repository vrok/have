package have

import (
	"bytes"
	"fmt"
	"regexp"
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
	if cc == forcedIndentChunk {
		return indent
	}

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

type forcedIndent struct{}

func (f forcedIndent) Generate(current *CodeChunk) {
	current.AddChunks(forcedIndentChunk)
}

var splitter = regexp.MustCompile("%i?C")
var forcedIndentChunk = &CodeChunk{}
var ForcedIndent = &forcedIndent{}

// Format with fmt.Sprintf, but one addition: "%C" can be use to add Generables.
// Use "%iC" variant for InlineGenerables.
// Indents are inserted automatically, but for multi-line statements that need
// extra indents to be added in the middle, use the ForcedIndent generable.
func (cc *CodeChunk) AddChprintf(format string, a ...interface{}) {
	var nonGenerables []interface{}

	//format = strings.Replace(format, "%I", cc.indent, -1)

	ops := splitter.FindAllString(format, -1)
	parts := splitter.Split(format, -1)

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

			switch ops[i] {
			case "%iC":
				v.(InlineGenerable).InlineGenerate(cc, false)
			case "%C":
				v.Generate(cc)
			}

			i++
		default:
			nonGenerables = append(nonGenerables, v)
		}
	}

	// Leftovers
	cc.AddString(fmt.Sprintf(parts[i], nonGenerables...))
}

type Generable interface {
	// Generate the full version of the output code.
	Generate(current *CodeChunk)
}

type InlineGenerable interface {
	Generable
	// Generate a shorter version of the code that e.g. fits into for-loop header.
	InlineGenerate(current *CodeChunk, noParenth bool)
}

type EmptyGenerable struct{}

func (eg EmptyGenerable) Generate(current *CodeChunk) {}

func (id *Ident) Generate(current *CodeChunk) {
	current.AddString(id.name)
}

func (n *NilExpr) Generate(current *CodeChunk) {
	current.AddString("nil")
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
	//current.AddString(fmt.Sprintf("(%s)(%s)", lit.typ, val))
	current.AddString(val)
}

func (lit *CompoundLit) Generate(current *CodeChunk) {
	current.AddChprintf("%s{", lit.typ)

	if lit.kind == COMPOUND_EMPTY {
		current.AddChprintf("}")
		return
	}

	current.AddChprintf("\n")
	ch := current.NewBlockChunk()

	switch lit.kind {
	case COMPOUND_LISTLIKE:
		for _, val := range lit.elems {
			ch.NewChunk().AddChprintf("%C,\n", val)
		}
	case COMPOUND_MAPLIKE:
		lit.eachKeyVal(func(key, val Expr) {
			ch.NewChunk().AddChprintf("%C: %C,\n", key, val)
		})
	default:
		panic("shouldn't ever happen, please report a bug")
	}

	current.AddChprintf("%C}", ForcedIndent)
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

	names := current.NewChunk()
	inits := current.NewChunk()

	i, count := 0, len(vd.Vars)
	vd.eachPair(func(v *Variable, init Expr) {
		names.AddChprintf("%s", v.name)
		if init != nil {
			inits.AddChprintf("(%s)(%C)", v.Type, init)
		} else {
			inits.AddChprintf("(%s)(%s)", v.Type, v.Type.ZeroValue())
		}
		if i+1 < count {
			names.AddChprintf(", ")
			inits.AddChprintf(", ")
		}
	})

	names.AddChprintf(" = ")
}

func (dc DeclChain) Generate(current *CodeChunk) {
	current = current.NewChunk()

	names := current.NewChunk()
	inits := current.NewChunk()

	i, count := 0, dc.countVars()
	dc.eachPair(func(v *Variable, init Expr) {
		names.AddChprintf("%s", v.name)
		if init != nil {
			inits.AddChprintf("(%s)(%C)", v.Type, init)
		} else {
			inits.AddChprintf("(%s)(%s)", v.Type, v.Type.ZeroValue())
		}
		if i+1 < count {
			names.AddChprintf(", ")
			inits.AddChprintf(", ")
		}
	})

	names.AddChprintf(" = ")
}

func (vs *VarStmt) Generate(current *CodeChunk) {
	if vs.IsFuncStmt {
		vs.Vars[0].Inits[0].(Generable).Generate(current)
		return
	}
	for _, vd := range vs.Vars {
		current.AddChprintf("var %C\n", vd)
	}
}

func (vs *VarStmt) InlineGenerate(current *CodeChunk, noParenth bool) {
	if vs == nil || len(vs.Vars) == 0 {
		return
	}

	i := 0
	vs.Vars.eachPair(func(vd *Variable, init Expr) {
		current.AddString(vd.name)
		if i+1 < len(vs.Vars) {
			current.AddString(", ")
		}
		i++
	})

	current.AddString(" := ")

	i = 0
	vs.Vars.eachPair(func(vd *Variable, init Expr) {
		current.AddChprintf("(%s)(%C)", vd.Type, init.(Generable))
		if i+1 < len(vs.Vars) {
			current.AddString(", ")
		}
		i++
	})
}

func (ds *DotSelector) Generate(current *CodeChunk) {
	current.AddChprintf("%C.%C", ds.Left, ds.Right)
}

func (as *AssignStmt) Generate(current *CodeChunk) {
	as.InlineGenerate(current, true)
	current.AddString("\n")
}

func (as *AssignStmt) InlineGenerate(current *CodeChunk, noParenth bool) {
	for i, v := range as.Lhs {
		v.(Generable).Generate(current)
		if i+1 < len(as.Lhs) {
			current.AddString(", ")
		}
	}

	current.AddChprintf(" %s ", as.Token.Value)

	for i, v := range as.Rhs {
		v.(Generable).Generate(current)
		if i+1 < len(as.Rhs) {
			current.AddString(", ")
		}
	}

}

func (ae *ArrayExpr) Generate(current *CodeChunk) {
	current.AddChprintf("%C[%C]", ae.Left, ae.Index)
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
	if fd.Receiver == nil {
		current.AddChprintf("func %s(", fd.name)
	} else {
		current.AddChprintf("func (self %s) %s(", fd.Receiver.Type, fd.name)
	}

	i := 0

	fd.Args.eachPair(func(arg *Variable, init Expr) {
		current.AddChprintf("%s %s", arg.name, arg.Type)
		if i+1 < len(fd.Args) {
			current.AddString(", ")
		}
		i++
	})
	current.AddString(")")

	if len(fd.Results) > 0 {
		current.AddString(" (")
		i = 0
		fd.Args.eachPair(func(arg *Variable, init Expr) {
			current.AddChprintf("%s %s", arg.name, arg.Type)
			if i+1 < len(fd.Args) {
				current.AddString(", ")
			}
			i++
		})
		current.AddString(")")
	}

	current.AddString(" {\n")
	fd.Code.Generate(current)

	current.AddString("}\n")
}

func (bl *CodeBlock) Generate(current *CodeChunk) {
	block := current.NewBlockChunk()
	for _, stmt := range bl.Statements {
		stmt.(Generable).Generate(block.NewChunk())
	}
}

func (es *ExprStmt) Generate(current *CodeChunk) {
	current.AddChprintf("%C\n", es.Expression.(Generable))
}

func (es *ExprStmt) InlineGenerate(current *CodeChunk, noParenth bool) {
	es.Expression.(Generable).Generate(current)
}

func (fs *IfStmt) Generate(current *CodeChunk) {
	current = current.NewChunk()

	if fs.Branches[0].ScopedVarDecl != nil {
		current.AddChprintf("if %iC; %C {\n", fs.Branches[0].ScopedVarDecl, fs.Branches[0].Condition)
	} else {
		current.AddChprintf("if %C {\n", fs.Branches[0].Condition)
	}

	fs.Branches[0].Code.Generate(current)
	current.AddChprintf("%C}", ForcedIndent)

	for i, branch := range fs.Branches {
		if i == 0 {
			continue // It's already generated
		}

		if branch.Condition != nil {
			current.AddChprintf(" else if %C {\n", branch.Condition)
		} else {
			current.AddChprintf(" else {\n")
		}

		branch.Code.Generate(current)
		current.AddChprintf("%C}", ForcedIndent)
	}

	current.AddString("\n")
}

func (fs *ForStmt) Generate(current *CodeChunk) {
	current = current.NewChunk()

	// TODO: Handle `for` variants other than 3-way
	current.AddChprintf("for %iC; %C; %iC {\n%C%C}\n", fs.ScopedVarDecl, fs.Condition, fs.RepeatStmt, fs.Code, ForcedIndent)
}

func (f *File) Generate(current *CodeChunk) {
	current.AddChprintf("package %s\n\n", f.Pkg)
	for _, stmt := range f.Statements {
		stmt.(Generable).Generate(current)
	}
}

func (bs *BranchStmt) Generate(current *CodeChunk) {
	var typ = ""
	switch bs.Token.Type {
	case TOKEN_BREAK:
		typ = "break"
	case TOKEN_CONTINUE:
		typ = "continue"
	case TOKEN_GOTO:
		typ = "goto"
	default:
		panic("impossible")
	}
	if bs.Right == nil {
		current.AddChprintf("%s\n", typ)
	} else {
		current.AddChprintf("%s %C\n", typ, bs.Right)
	}
}

func (ls *LabelStmt) Generate(current *CodeChunk) {
	current.AddChprintf("%s:\n", ls.Name())
}

func (ss *StructStmt) Generate(current *CodeChunk) {
	current.AddChprintf("type %s struct {\n", ss.Struct.Name)

	ch := current.NewBlockChunk()
	for _, name := range ss.Struct.Keys {
		if _, ok := ss.Struct.Members[name]; !ok {
			// Not a plain member, but a method
			continue
		}
		ch.AddChprintf("%s %s\n", name, ss.Struct.Members[name])
	}

	current.AddChprintf("%C}\n\n", ForcedIndent)

	for _, name := range ss.Struct.Keys {
		if _, ok := ss.Struct.Methods[name]; !ok {
			// Not a method, a plain member
			continue
		}
		current.AddChprintf("%C\n", ss.Struct.Methods[name])
	}
}

func (is *IfaceStmt) Generate(current *CodeChunk) {
	current.AddChprintf("type %s %s\n", is.Iface.name, is.Iface)
}

// TODO: Now just write Generables for all statements/expressions and we're done...
