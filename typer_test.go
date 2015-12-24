package have

import (
	"fmt"
	"testing"
)

func TestSimple(t *testing.T) {
	code := `var a int = 1`

	parser := NewParser(NewLexer([]rune(code)))
	result, err := parser.parseVarStmt()
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	err = result.Vars[0].NegotiateTypes()
	fmt.Printf("ZZZ err: %s\nresult: %#v\n", err, result)
	t.Fail()
}
