package have

import "fmt"

type needsDepErr struct {
	depName   string
	unchecked []ExprToProcess
}

func (e *needsDepErr) Error() string {
	return fmt.Sprintf("Object not typechecked yet: %s", e.depName)
}
