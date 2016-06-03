package have

import (
	"fmt"
	"go/token"
)

type File struct {
	name, code, pkg string

	statements []*TopLevelStmt
	objects    map[string]Object
	tfile      *token.File
	parser     *Parser
}

func NewFile(name, code string, tfile *token.File) *File {
	return &File{name: name,
		code:    code,
		tfile:   tfile,
		objects: make(map[string]Object)}
}

func (f *File) Parse() []error {
	f.parser = NewParser(NewLexer([]rune(f.code)))
	err := f.parser.ParseFile(f)
	if err != nil {
		return []error{err}
	}
	if len(*(f.parser.identStack)) != 2 {
		// There should be 2 scopes - one for the builtins, one for the top-level package
		// declarations.
		return []error{fmt.Errorf("Internal compiler error: wrong number of scopes")}
	}
	f.objects = (*f.parser.identStack)[1]
	return nil
}

func (f *File) Typecheck() []error {
	for _, stmt := range f.statements {
		typedStmt := stmt.Stmt.(ExprToProcess)
		if err := typedStmt.NegotiateTypes(); err != nil {
			return []error{err}
		}

		//f.statements = append(f.statements, stmt)
	}
	return nil
}

func (f *File) ParseAndCheck() []error {
	errors := f.Parse()
	if len(errors) > 0 {
		return errors
	}
	errors = f.Typecheck()
	if len(errors) > 0 {
		return errors
	}

	return nil
}

func (f *File) GenerateCode() string {
	cc := &CodeChunk{}
	f.Generate(cc)
	return cc.ReadAll()
}
