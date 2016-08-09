package have

import "fmt"

type CompileError struct {
	Message, File string
	Line, Column  int
}

//func NewCompileError(t *Token Message,

func (ce *CompileError) Error() string {
	return fmt.Sprintf("%s:%d: %s", ce.File, ce.Line, ce.Message)
}
