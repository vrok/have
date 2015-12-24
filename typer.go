// Negotiate and validate types in an AST.
package have

import "fmt"

type ExprToProcess interface {
	Expr
	NegotiateTypes() error
}

type TypedExpr interface {
	//ExprToProcess
	Expr

	//TypeFullyKnown() bool
	Type() Type
	//CanBeOfType(typ Type) bool
	ApplyType(typ Type) error
	GuessType() (ok bool, typ Type)
}

func (vd *VarDecl) NegotiateTypes() error {
	typedInit := vd.Init.(TypedExpr)

	fmt.Printf("ZZZ 1 %#v ---- %#v\n", vd, typedInit)

	typ, err := NegotiateTypes(vd.Type, typedInit.Type())
	if err != nil {
		// Try guessing. Literals like "1", or "{1, 2}" can be used
		// to initialize variables of many types (int/double/etc,
		// array/slice/struct), but if the type of the variable is
		// unknown, we try to guess the type (for these examples
		// it would be "int" and "[]int").
		ok, guessedType := typedInit.GuessType()
		if ok {
			typ, err = NegotiateTypes(vd.Type, guessedType)
		}

		if err != nil {
			return err
		}
	}

	return typedInit.ApplyType(typ)
	//if !typedInit.CanBeOfType(typ) {
	//	return fmt.Errorf("Couldn't infer type of %s", vd.Name)
	//}

	//// TODO: we've got the type, now apply it

	//return nil
}

func (ex *BlankExpr) Type() Type                     { panic("nope") }
func (ex *BlankExpr) ApplyType(typ Type) error       { panic("nope") }
func (ex *BlankExpr) GuessType() (ok bool, typ Type) { panic("nope") }

func (ex *CompoundLit) Type() Type                     { panic("nope") }
func (ex *CompoundLit) ApplyType(typ Type) error       { panic("nope") }
func (ex *CompoundLit) GuessType() (ok bool, typ Type) { panic("nope") }

func (ex *BinaryOp) Type() Type                     { panic("nope") }
func (ex *BinaryOp) ApplyType(typ Type) error       { panic("nope") }
func (ex *BinaryOp) GuessType() (ok bool, typ Type) { panic("nope") }

func (ex *UnaryOp) Type() Type                     { panic("nope") }
func (ex *UnaryOp) ApplyType(typ Type) error       { panic("nope") }
func (ex *UnaryOp) GuessType() (ok bool, typ Type) { panic("nope") }

func (ex *Ident) Type() Type {
	if ex.varDecl != nil {
		return ex.varDecl.Type
	}
	return nil
}

func (ex *Ident) ApplyType(typ Type) error {
	return fmt.Errorf("Identifier %s is of type %s", ex.name, ex.varDecl.Name)
}

func (ex *Ident) GuessType() (ok bool, typ Type) {
	return false, nil
}

func (ex *BasicLit) Type() Type {
	return ex.typ
}

func (ex *BasicLit) ApplyType(typ Type) error {
	actualType := typ
	if typ.Kind() == KIND_CUSTOM {
		actualType = typ.(*CustomType).RootType()
	}

	switch {
	case ex.token.Type == TOKEN_STR &&
		actualType.Kind() == KIND_SIMPLE &&
		actualType.(*SimpleType).ID == SIMPLE_TYPE_STRING:
		fallthrough
	case ex.token.Type == TOKEN_NUM &&
		actualType.Kind() == KIND_SIMPLE &&
		actualType.(*SimpleType).ID == SIMPLE_TYPE_INT:
		fallthrough
	case (ex.token.Type == TOKEN_TRUE || ex.token.Type == TOKEN_FALSE) &&
		actualType.Kind() == KIND_SIMPLE &&
		actualType.(*SimpleType).ID == SIMPLE_TYPE_BOOL:

		ex.typ = typ
	}
	return fmt.Errorf("Can't use this literal for this type")
}

func (ex *BasicLit) GuessType() (ok bool, typ Type) {
	switch ex.token.Type {
	case TOKEN_STR:
		return true, &SimpleType{ID: SIMPLE_TYPE_STRING}
	case TOKEN_NUM:
		// TODO: handle anything else than just integers
		return true, &SimpleType{ID: SIMPLE_TYPE_INT}
	case TOKEN_TRUE, TOKEN_FALSE:
		return true, &SimpleType{ID: SIMPLE_TYPE_BOOL}
	}
	return false, nil
}

func (t *SimpleType) Negotiate(other Type) (Type, error) {
	if other, ok := other.(*SimpleType); !ok {
		return nil, fmt.Errorf("Not a simple type")
	} else if t.ID == other.ID {
		return t, nil
	} else {
		return nil, fmt.Errorf("Different simple types, %s and %s",
			simpleTypeAsStr[t.ID], simpleTypeAsStr[other.ID])
	}
}

func (t *ArrayType) Negotiate(other Type) (Type, error) {
	if other, ok := other.(*ArrayType); !ok {
		return nil, fmt.Errorf("Not an array type")
	} else if t.Size == other.Size {
		typ, err := NegotiateTypes(t.Of, other.Of)
		if err != nil {
			return nil, err
		}
		return &ArrayType{Size: t.Size, Of: typ}, nil
	} else {
		return nil, fmt.Errorf("Different array sizes, %d and %d", t.Size, other.Size)
	}
}

func (t *SliceType) Negotiate(other Type) (Type, error) {
	if other, ok := other.(*SliceType); !ok {
		return nil, fmt.Errorf("Not a slice type")
	} else {
		typ, err := NegotiateTypes(t.Of, other.Of)
		if err != nil {
			return nil, err
		}
		return &SliceType{Of: typ}, nil
	}
}

func (t *CustomType) Negotiate(other Type) (Type, error) {
	panic("todo")
}

func (t *UnknownType) Negotiate(other Type) (Type, error) {
	panic("todo")
}

func (t *PointerType) Negotiate(other Type) (Type, error) {
	panic("todo")
}

func (t *MapType) Negotiate(other Type) (Type, error) {
	panic("todo")
}

func (t *StructType) Negotiate(other Type) (Type, error) {
	panic("todo")
}

func NegotiateTypes(t1, t2 Type) (Type, error) {
	if t1.Known() && t2.Known() {
		// Both types are fully known, no negotiation needed, just
		// check if they are the same.
		if t1.String() != t2.String() {
			return nil, fmt.Errorf("Wanted type %s, but got type %s",
				t1.String(), t2.String())
		}
		return t1, nil
	}

	if t1.Kind() == t2.Kind() {
		return t1.Negotiate(t2)
	} else if t1.Kind() == KIND_UNKNOWN {
		if !t2.Known() {
			// If one type is completely unknown, the other has to be fully known.
			return nil, fmt.Errorf("Couldn't infer type, too little information")
		}
		return t2, nil
	} else if t2.Kind() == KIND_UNKNOWN {
		if !t1.Known() {
			// If one type is completely unknown, the other has to be fully known.
			return nil, fmt.Errorf("Couldn't infer type, too little information")
		}
		return t1, nil
	}
	// TODO: use stringer
	return nil, fmt.Errorf("Different type kinds: %#v and %#v", t1.Kind(), t2.Kind())
}
