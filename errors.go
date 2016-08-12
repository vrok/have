package have

import "fmt"

import gotoken "go/token"

type CompileError struct {
	Message string
	Pos     gotoken.Pos
}

func CompileErrorf(token *Token, message string, args ...interface{}) *CompileError {
	return &CompileError{
		Message: fmt.Sprintf(message, args...),
		Pos:     token.Pos,
	}
}

func ExprErrorf(expr Expr, message string, args ...interface{}) *CompileError {
	return &CompileError{
		Message: fmt.Sprintf(message, args...),
		Pos:     expr.Pos(),
	}
}

func (ce *CompileError) Error() string {
	return ce.Message
}

func (ce *CompileError) PrettyString(fset *gotoken.FileSet) string {
	position := fset.Position(ce.Pos)
	return fmt.Sprintf("%s:%d: %s", position.Filename, position.Line, ce.Message)
}
