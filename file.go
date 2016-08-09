package have

import gotoken "go/token"

type PkgLocator interface {
	Locate(pkgPath string) ([]*File, error)
}

type File struct {
	name, code, pkg string
	size            int

	statements []*TopLevelStmt
	objects    map[string]Object
	tfile      *gotoken.File
	parser     *Parser
	tc         *TypesContext
}

func NewFile(name, code string, tc *TypesContext, tfile *gotoken.File) *File {
	return &File{name: name,
		code:    code,
		tc:      tc,
		tfile:   tfile,
		size:    len(code),
		objects: make(map[string]Object)}
}

func (f *File) Parse() []error {
	f.parser = NewParser(NewLexer([]rune(f.code), f.tfile, 0))
	err := f.parser.ParseFile(f)
	if err != nil {
		return []error{err}
	}
	f.objects = f.parser.topLevelDecls
	return nil
}

func (f *File) Typecheck() []error {
	for _, stmt := range f.statements {
		typedStmt := stmt.Stmt.(ExprToProcess)
		if err := typedStmt.NegotiateTypes(f.tc); err != nil {
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
	f.Generate(f.tc, cc)
	return cc.ReadAll()
}
