package have

import (
	"fmt"
	"testing"
)

func TestPrimaryExpr(t *testing.T) {
	//in := []rune("test")
	in := []rune("test.tere[123]")
	//out := &Ident{"test"}

	parser := NewParser(NewLexer(in))
	fmt.Printf("GOT %#v\n", parser.parsePrimaryExpr())
}
