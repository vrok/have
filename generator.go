package have

import (
	"bytes"
	"fmt"
	"regexp"
	"sort"
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

func (f forcedIndent) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChunks(forcedIndentChunk)
}

var splitter = regexp.MustCompile("%i?C")
var forcedIndentChunk = &CodeChunk{}
var ForcedIndent = &forcedIndent{}

// Format using fmt.Sprintf, but with one addition: "%C" can be used to add Generables.
// Use "%iC" variant for InlineGenerables.
// Indents are inserted automatically, but for multi-line statements that need
// extra indents in the middle, use the ForcedIndent generable.
func (cc *CodeChunk) AddChprintf(tc *TypesContext, format string, a ...interface{}) {
	var nonGenerables []interface{}

	ops := splitter.FindAllString(format, -1)
	parts := splitter.Split(format, -1)

	i := 0
	for _, arg := range a {
		if arg == nil {
			arg = EmptyGenerable{}
		}

		switch v := arg.(type) {
		case Generable:
			cc.AddString(fmt.Sprintf(parts[i], nonGenerables...))
			nonGenerables = nil

			switch ops[i] {
			case "%iC":
				ig, ok := v.(InlineGenerable)
				if ok {
					ig.InlineGenerate(tc, cc, false)
				} else {
					// Fallback to Generable
					v.Generate(tc, cc)
				}
			case "%C":
				v.Generate(tc, cc)
			}

			i++
		default:
			if genericType, ok := v.(*GenericType); ok {
				// We can't use GenericType.String(), it would return a valid Go type name.
				// TODO: This could be refactored to avoid such special treatment.
				v = genericType.Struct.Name
			}
			nonGenerables = append(nonGenerables, v)
		}
	}

	// Leftovers
	cc.AddString(fmt.Sprintf(parts[i], nonGenerables...))
}

type Generable interface {
	// Generate the full version of the output code.
	Generate(tc *TypesContext, current *CodeChunk)
}

type InlineGenerable interface {
	Generable
	// Generate a shorter version of the code that e.g. fits into for-loop header.
	InlineGenerate(tc *TypesContext, current *CodeChunk, noParenth bool)
}

type EmptyGenerable struct{}

func (eg EmptyGenerable) Generate(tc *TypesContext, current *CodeChunk)                       {}
func (vs EmptyGenerable) InlineGenerate(tc *TypesContext, current *CodeChunk, noParenth bool) {}

func (i *ImportStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChprintf(tc, "import %s \"%s\"\n", i.name, i.path)
}

func (id *Ident) Generate(tc *TypesContext, current *CodeChunk) {
	if alias, ok := tc.goNames[id]; ok {
		current.AddChprintf(tc, alias)
		return
	}
	current.AddString(id.name)
}

func (n *NilExpr) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddString("nil")
}

func (n *PassStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddString("// pass\n")
}

func (lit *BasicLit) Generate(tc *TypesContext, current *CodeChunk) {
	val := ""
	switch lit.token.Type {
	case TOKEN_TRUE:
		current.AddString("true")
		return
	case TOKEN_FALSE:
		current.AddString("false")
		return
	case TOKEN_INT, TOKEN_STR, TOKEN_RUNE:
		val = lit.token.Value.(string)
	default:
		panic("impossible")
	}

	// Don't generate type conversions if not necessary.
	//current.AddString(fmt.Sprintf("(%s)(%s)", lit.typ, val))
	current.AddString(val)
}

func (lit *CompoundLit) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChprintf(tc, "%s{", lit.typ)

	if lit.kind == COMPOUND_EMPTY {
		current.AddChprintf(tc, "}")
		return
	}

	current.AddChprintf(tc, "\n")
	ch := current.NewBlockChunk()

	switch lit.kind {
	case COMPOUND_LISTLIKE:
		for _, val := range lit.elems {
			ch.NewChunk().AddChprintf(tc, "%C,\n", val)
		}
	case COMPOUND_MAPLIKE:
		lit.eachKeyVal(func(key, val Expr) {
			ch.NewChunk().AddChprintf(tc, "%C: %C,\n", key, val)
		})
	default:
		panic("shouldn't ever happen, please report a bug")
	}

	current.AddChprintf(tc, "%C}", ForcedIndent)
}

func (op *UnaryOp) Generate(tc *TypesContext, current *CodeChunk) {
	// TODO: Put the right operator in
	current.AddChprintf(tc, "(%s%C)", op.op.Value.(string), op.Right.(Generable))
}

func (op *BinaryOp) Generate(tc *TypesContext, current *CodeChunk) {
	// TODO: Put the right operator in
	current.AddChprintf(tc, "(%C %s %C)", op.Left.(Generable), op.op.Value.(string), op.Right.(Generable))
}

func (td *TypeDecl) Generate(tc *TypesContext, current *CodeChunk) {
	current = current.NewChunk()
	current.AddChprintf(tc, "type %s %s\n", td.Name(), td.AliasedType)
}

func (vd *VarDecl) Generate(tc *TypesContext, current *CodeChunk) {
	current = current.NewChunk()

	names := current.NewChunk()
	inits := current.NewChunk()

	i, count := 0, len(vd.Vars)
	noMoreInits := false

	vd.eachPair(func(v *Variable, init Expr) {
		names.AddChprintf(tc, "%s", v.name)

		var it Type
		if init != nil {
			it, _ = init.(TypedExpr).Type(tc)
		}

		if noMoreInits {
		} else if init != nil && it.Kind() == KIND_TUPLE {
			// For tuples we can't do any type casting/conversion. It is a bit magical
			// in Go, nothing can be in between.
			inits.AddChprintf(tc, "%C", init)
			noMoreInits = true
		} else {
			if init != nil {
				inits.AddChprintf(tc, "(%s)(%C)", v.Type, init)
			} else {
				inits.AddChprintf(tc, "(%s)(%s)", v.Type, v.Type.ZeroValue())
			}
		}

		if i+1 < count {
			names.AddChprintf(tc, ", ")
			if !noMoreInits {
				inits.AddChprintf(tc, ", ")
			}
		}
		i++
	})

	names.AddChprintf(tc, " = ")
}

func (dc DeclChain) Generate(tc *TypesContext, current *CodeChunk) {
	current = current.NewChunk()

	names := current.NewChunk()
	inits := current.NewChunk()

	i, count := 0, dc.countVars()
	dc.eachPair(func(v *Variable, init Expr) {
		names.AddChprintf(tc, "%s", v.name)
		if init != nil {
			inits.AddChprintf(tc, "(%s)(%C)", v.Type, init)
		} else {
			inits.AddChprintf(tc, "(%s)(%s)", v.Type, v.Type.ZeroValue())
		}
		if i+1 < count {
			names.AddChprintf(tc, ", ")
			inits.AddChprintf(tc, ", ")
		}
		i++
	})

	names.AddChprintf(tc, " = ")
}

func (vs *VarStmt) Generate(tc *TypesContext, current *CodeChunk) {
	if vs.IsFuncStmt {
		vs.Vars[0].Inits[0].(Generable).Generate(tc, current)
		return
	}
	for i, vd := range vs.Vars {
		current.AddChprintf(tc, "var %C\n", vd)
		if i+1 < len(vs.Vars) {
			current.AddChprintf(tc, "%C", ForcedIndent)
		}
	}
}

func (vs *VarStmt) InlineGenerate(tc *TypesContext, current *CodeChunk, noParenth bool) {
	if vs == nil || len(vs.Vars) == 0 {
		return
	}

	i := 0
	left, right := current.NewChunk(), current.NewChunk()

	vs.Vars.eachPair(func(vd *Variable, init Expr) {
		left.AddString(vd.name)
		right.AddChprintf(tc, "(%s)(%C)", vd.Type, init.(Generable))

		if i+1 < len(vs.Vars) {
			left.AddString(", ")
			right.AddString(", ")
		}

		i++
	})

	left.AddString(" := ")
}

func (ds *DotSelector) Generate(tc *TypesContext, current *CodeChunk) {
	if alias, ok := tc.goNames[ds]; ok {
		current.AddChprintf(tc, alias)
		return
	}
	current.AddChprintf(tc, "%C.%C", ds.Left, ds.Right)
}

func (ta *TypeAssertion) Generate(tc *TypesContext, current *CodeChunk) {
	if ta.ForSwitch {
		current.AddChprintf(tc, "%C.(type)", ta.Left)
	} else {
		current.AddChprintf(tc, "%C.(%s)", ta.Left, ta.Right.typ)
	}
}

func (as *AssignStmt) Generate(tc *TypesContext, current *CodeChunk) {
	as.InlineGenerate(tc, current, true)
	current.AddString("\n")
}

func (as *AssignStmt) InlineGenerate(tc *TypesContext, current *CodeChunk, noParenth bool) {
	for i, v := range as.Lhs {
		v.(Generable).Generate(tc, current)
		if i+1 < len(as.Lhs) {
			current.AddString(", ")
		}
	}

	current.AddChprintf(tc, " %s ", as.Token.Value)

	for i, v := range as.Rhs {
		v.(Generable).Generate(tc, current)
		if i+1 < len(as.Rhs) {
			current.AddString(", ")
		}
	}
}

func (ae *ArrayExpr) Generate(tc *TypesContext, current *CodeChunk) {
	if alias, ok := tc.goNames[ae]; ok {
		current.AddChprintf(tc, alias)
		return
	}

	current.AddChprintf(tc, "%C[", ae.Left)
	for i, arg := range ae.Index {
		current.AddChprintf(tc, "%iC", arg)
		if i+1 < len(ae.Index) {
			current.AddString(", ")
		}
	}
	current.AddChprintf(tc, "]")
}

func (se *SliceExpr) Generate(tc *TypesContext, current *CodeChunk) {
	if se.From != nil {
		current.AddChprintf(tc, "%C", se.From)
	}

	current.AddChprintf(tc, ":")

	if se.To != nil {
		current.AddChprintf(tc, "%C", se.To)
	}

	// TODO: third component
}

func (fc *FuncCallExpr) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChprintf(tc, "%iC(", fc.Left.(Generable))
	for i, arg := range fc.Args {
		current.AddChprintf(tc, "%iC", arg)
		if i+1 < len(fc.Args) {
			current.AddString(", ")
		}
	}
	current.AddString(")")
}

func (fd *FuncDecl) Generate(tc *TypesContext, current *CodeChunk) {
	fd.InlineGenerate(tc, current, true)
	current.AddChprintf(tc, "\n")
}

func (fd *FuncDecl) InlineGenerate(tc *TypesContext, current *CodeChunk, noParenth bool) {
	current = current.NewChunk()
	if fd.Receiver == nil {
		current.AddChprintf(tc, "func %s(", fd.name)
	} else {
		current.AddChprintf(tc, "func (self %s) %s(", fd.Receiver.Type, fd.name)
	}

	i := 0

	fd.Args.eachPair(func(arg *Variable, init Expr) {
		current.AddChprintf(tc, "%s %s", arg.name, arg.Type)
		if i+1 < fd.Args.countVars() {
			current.AddString(", ")
		}
		i++
	})
	current.AddString(")")

	if len(fd.Results) > 0 {
		current.AddString(" (")
		i = 0
		fd.Results.eachPair(func(arg *Variable, init Expr) {
			if arg.name == "" {
				current.AddChprintf(tc, "%s", arg.Type)
			} else {
				current.AddChprintf(tc, "%s %s", arg.name, arg.Type)
			}
			if i+1 < fd.Results.countVars() {
				current.AddString(", ")
			}
			i++
		})
		current.AddString(")")
	}

	current.AddString(" {\n")
	fd.Code.Generate(tc, current)

	current.AddChprintf(tc, "%C}", ForcedIndent)
}

func (bl *CodeBlock) Generate(tc *TypesContext, current *CodeChunk) {
	block := current.NewBlockChunk()
	for _, stmt := range bl.Statements {
		stmt.(Generable).Generate(tc, block.NewChunk())
	}
}

func (es *ExprStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChprintf(tc, "%C\n", es.Expression.(Generable))
}

func (es *ExprStmt) InlineGenerate(tc *TypesContext, current *CodeChunk, noParenth bool) {
	es.Expression.(Generable).Generate(tc, current)
}

func (fs *IfStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current = current.NewChunk()

	if fs.Branches[0].ScopedVar != nil {
		current.AddChprintf(tc, "if %iC; %C {\n", fs.Branches[0].ScopedVar, fs.Branches[0].Condition)
	} else {
		current.AddChprintf(tc, "if %C {\n", fs.Branches[0].Condition)
	}

	fs.Branches[0].Code.Generate(tc, current)
	current.AddChprintf(tc, "%C}", ForcedIndent)

	for i, branch := range fs.Branches {
		if i == 0 {
			continue // It's already generated
		}

		if branch.Condition != nil {
			current.AddChprintf(tc, " else if %C {\n", branch.Condition)
		} else {
			current.AddChprintf(tc, " else {\n")
		}

		branch.Code.Generate(tc, current)
		current.AddChprintf(tc, "%C}", ForcedIndent)
	}

	current.AddString("\n")
}

func (ss *SwitchStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current = current.NewChunk()

	if ss.ScopedVar != nil {
		current.AddChprintf(tc, "switch %iC; %iC {\n", ss.ScopedVar, ss.Value)
	} else {
		current.AddChprintf(tc, "switch %iC {\n", ss.Value)
	}

	for _, branch := range ss.Branches {
		if len(branch.Values) == 0 {
			current.AddChprintf(tc, "default:\n")
		} else {
			current.AddChprintf(tc, "case ")

			for i, val := range branch.Values {
				current.AddChprintf(tc, "%C", val)
				if i+1 < len(branch.Values) {
					current.AddChprintf(tc, ", ")
				}
			}

			current.AddChprintf(tc, ":\n")
		}

		branch.Code.Generate(tc, current)
	}

	current.AddChprintf(tc, "}\n")
}

func (fs *ForStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current = current.NewChunk()

	if fs.ScopedVar == nil && fs.RepeatStmt == nil {
		current.AddChprintf(tc, "for %C {\n%C%C}\n", fs.Condition, fs.Code, ForcedIndent)
	} else {
		current.AddChprintf(tc, "for %iC; %C; %iC {\n%C%C}\n", fs.ScopedVar, fs.Condition, fs.RepeatStmt, fs.Code, ForcedIndent)
	}
}

func (fs *ForRangeStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current = current.NewChunk()
	current.AddChprintf(tc, "for ")

	if fs.ScopedVars != nil {
		var namesList []string
		fs.ScopedVars.eachPair(func(v *Variable, init Expr) {
			namesList = append(namesList, v.Name())
		})

		names := strings.Join(namesList, ", ")
		current.AddChprintf(tc, "%s := range %iC {\n%C\t%s := %s // Added by compiler\n%C%C}\n",
			names, fs.Series, ForcedIndent, names, names, fs.Code, ForcedIndent)
	} else if fs.OutsideVars != nil {
		i := 0
		for _, expr := range fs.OutsideVars {
			if i+1 < len(fs.OutsideVars) {
				current.AddChprintf(tc, "%iC, ", expr)
			} else {
				current.AddChprintf(tc, "%iC", expr)
			}
			i++
		}
		current.AddChprintf(tc, " = range %iC {\n%C%C}\n", fs.Series, fs.Code, ForcedIndent)
	} else {
		panic("Internal error")
	}
}

func (f *File) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChprintf(tc, "package %s\n\n", f.pkg)
	for _, stmt := range f.statements {
		stmt.Stmt.(Generable).Generate(tc, current)
	}
}

func (bs *BranchStmt) Generate(tc *TypesContext, current *CodeChunk) {
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
		current.AddChprintf(tc, "%s\n", typ)
	} else {
		current.AddChprintf(tc, "%s %C\n", typ, bs.Right)
	}
}

func (rs *ReturnStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChprintf(tc, "return")
	for i, v := range rs.Values {
		current.AddChprintf(tc, " %C", v)
		if i+1 < len(rs.Values) {
			current.AddChprintf(tc, ",")
		}
	}
	current.AddChprintf(tc, "\n")
}

func (ls *LabelStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChprintf(tc, "%s:\n", ls.Name())
}

func generateStruct(tc *TypesContext, current *CodeChunk, st *StructType) {
	current.AddChprintf(tc, "type %s struct {\n", st.Name)

	ch := current.NewBlockChunk()
	for _, name := range st.Keys {
		if _, ok := st.Members[name]; !ok {
			// Not a plain member, but a method
			continue
		}
		ch.AddChprintf(tc, "%s %s\n", name, st.Members[name])
	}

	current.AddChprintf(tc, "%C}\n\n", ForcedIndent)

	for _, name := range st.Keys {
		if _, ok := st.Methods[name]; !ok {
			// Not a method, a plain member
			continue
		}
		current.AddChprintf(tc, "%C\n", st.Methods[name])
	}
}

func (ss *StructStmt) Generate(tc *TypesContext, current *CodeChunk) {
	generateStruct(tc, current, ss.Struct)
}

func (is *IfaceStmt) Generate(tc *TypesContext, current *CodeChunk) {
	current.AddChprintf(tc, "type %s %s\n", is.Iface.name, is.Iface)
}

type instList []*Instantiation

func (l instList) Len() int           { return len(l) }
func (l instList) Less(i, j int) bool { return l[i].getGoName() < l[j].getGoName() }
func (l instList) Swap(i, j int)      { l[i], l[j] = l[j], l[i] }

func (gf *GenericFunc) Generate(tc *TypesContext, current *CodeChunk) {
	var insts instList
	for _, inst := range tc.instantiations {
		if inst.Generic == gf {
			insts = append(insts, inst)
		}
	}

	sort.Sort(insts)

	for _, inst := range insts {
		current.AddChprintf(tc, "// Generic instantiation\n")
		if inst.Init != nil {
			current.AddChprintf(tc, "%C\n", inst.Init)
		} else {
			panic("todo")
		}
	}
}

func (gs *GenericStruct) Generate(tc *TypesContext, current *CodeChunk) {
	var insts instList
	for _, inst := range tc.instantiations {
		if inst.Generic == gs {
			insts = append(insts, inst)
		}
	}

	sort.Sort(insts)

	for _, inst := range insts {
		current.AddChprintf(tc, "// Generic instantiation\n")
		generateStruct(tc, current, inst.Object.(*TypeDecl).AliasedType.(*StructType))
	}
}

// TODO: Now just write Generables for all statements/expressions and we're done...
