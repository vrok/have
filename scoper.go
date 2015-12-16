// Scoper operates on a raw AST and onnects variable occurences
// to their declarations, type usages to their definitions, etc.
package have

type Scoper struct {
	StackOfScopes []Scope
	ast           Scope
}

type Scope interface {
	FindVar(name string) *VarDecl
	Process(higherScopes []Scope) error
}

func (s *Scoper) NewScoper(ast Scope) *Scoper {
	return &Scoper{
		ast: ast,
	}
}

func (s *Scoper) Process() error {
	return nil
}
