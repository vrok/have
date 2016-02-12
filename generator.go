package have

import (
	"bytes"
	"fmt"
	"strings"
)

// CodeChunk can either be a string of smaller CodeChunks
// or its value can be a string.
// Think of it as of a union type.
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
}

// TODO: implement it in a io.Reader form, not keeping all results in memory
func (cc *CodeChunk) ReadAll() string {
	if cc.code != "" {
		return cc.code
	}

	buf := bytes.Buffer{}
	for _, chk := range cc.chunks {
		buf.WriteString(chk.ReadAll())
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
	return &CodeChunk{code: s, parent: cc}
}

// Format with fmt.Sprintf, but one addition: "%C" can be use to add Generables.
func (cc *CodeChunk) AddChprintf(format string, a ...interface{}) {
	var nonGenerables []interface{}

	parts := strings.Split(format, "%C")

	i := 0
	for _, arg := range a {
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

func (id *Ident) Generate(current *CodeChunk) {
	current.AddString(id.name)
}

func (lit *BasicLit) Generate(current *CodeChunk) {
	val := ""
	switch lit.token.Type {
	case TOKEN_NUM, TOKEN_TRUE, TOKEN_FALSE:
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
	current.AddChprintf("(%s(%C))", "+", op.Right.(Generable))
}

func (op *BinaryOp) Generate(current *CodeChunk) {
	// TODO: Put the right operator in
	current.AddChprintf("((%C)%s(%C))", op.Left.(Generable), "+", op.Right.(Generable))
}

func (vd *VarDecl) Generate(current *CodeChunk) {
	current.AddChprintf("var %s %s = (%C)\n", vd.name, vd.Type, vd.Init.(Generable))
}

func (vs *VarStmt) Generate(current *CodeChunk) {
	for _, vd := range vs.Vars {
		vd.Generate(current)
	}
}

// TODO: Now just write Generables for all statements/expressions and we're done...
