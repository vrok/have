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

	Type() Type
	ApplyType(typ Type) error
	GuessType() (ok bool, typ Type)
}

func unNilType(t *Type) Type {
	if *t == nil {
		*t = &UnknownType{}
	}
	return *t
}

func nonilTyp(t Type) Type {
	if t == nil {
		return &UnknownType{}
	}
	return t
}

// Implements the definition of underlying types from the Go spec.
func UnderlyingType(t Type) Type {
	if t.Kind() == KIND_CUSTOM {
		return t.(*CustomType).Decl.AliasedType
	}
	return t
}

// Implements the definition of named types from the Go spec.
func IsNamed(t Type) bool {
	return t.Kind() == KIND_CUSTOM || t.Kind() == KIND_SIMPLE
}

// Implements the definition of unnamed types from the Go spec.
func IsUnnamed(t Type) bool {
	return !IsNamed(t)
}

// Implements the definition of assignability from the Go spec.
func IsAssignable(to, what Type) bool {
	if IsNamed(to) && IsNamed(what) {
		return to.String() == what.String()
	}
	// TODO: handle other cases (nils, interfaces, etc.)
	return UnderlyingType(to).String() == UnderlyingType(what).String()
}

func (vs *VarStmt) NegotiateTypes() error {
	for _, v := range vs.Vars {
		err := v.NegotiateTypes()
		if err != nil {
			return err
		}
	}
	return nil
}

// This will overwrite the type pointer by varType.
func NegotiateExprType(varType *Type, value TypedExpr) error {
	*varType = nonilTyp(*varType)

	typ, err := NegotiateTypes(*varType, value.Type())
	if err != nil || !typ.Known() {
		// Try guessing. Literals like "1", or "{1, 2}" can be used
		// to initialize variables of many types (int/double/etc,
		// array/slice/struct), but if the type of the variable is
		// unknown, we try to guess the type (for these examples
		// it would be "int" and "[]int").
		ok, guessedType := value.GuessType()
		if ok {
			typ, err = NegotiateTypes(*varType, guessedType)
		}

		if err != nil {
			return err
		}
	}

	*varType = typ
	return value.ApplyType(typ)
}

func CheckCondition(expr TypedExpr) error {
	var boolTyp Type = &SimpleType{SIMPLE_TYPE_BOOL}

	err := NegotiateExprType(&boolTyp, expr)
	if err != nil {
		return err
	}

	if boolTyp.Kind() != KIND_SIMPLE || boolTyp.(*SimpleType).ID != SIMPLE_TYPE_BOOL {
		fmt.Errorf("Error while negotiating types")
	}
	return nil
}

func (is *IfStmt) NegotiateTypes() error {
	for _, b := range is.Branches {
		if b.ScopedVarDecl != nil {
			err := b.ScopedVarDecl.NegotiateTypes()
			if err != nil {
				return err
			}
		}

		if b.Condition != nil {
			if err := CheckCondition(b.Condition.(TypedExpr)); err != nil {
				return err
			}
		}

		if err := b.Code.CheckTypes(); err != nil {
			return err
		}
	}
	return nil
}

func (p *PassStmt) NegotiateTypes() error {
	return nil
}

func (fs *ForStmt) NegotiateTypes() error {
	for _, decl := range fs.ScopedVarDecls {
		if err := decl.NegotiateTypes(); err != nil {
			return err
		}
	}

	if fs.Condition != nil {
		if err := CheckCondition(fs.Condition.(TypedExpr)); err != nil {
			return err
		}
	}

	if fs.RepeatExpr != nil {

		uk := Type(&UnknownType{})
		if err := NegotiateExprType(&uk, fs.RepeatExpr.(TypedExpr)); err != nil {
			return err
		}
	}

	if err := fs.Code.CheckTypes(); err != nil {
		return err
	}

	return nil
}

func (vd *VarDecl) NegotiateTypes() error {
	return NegotiateExprType(&vd.Type, vd.Init.(TypedExpr))
}

func (es *ExprStmt) NegotiateTypes() error {
	uk := Type(&UnknownType{})
	return NegotiateExprType(&uk, es.Expression.(TypedExpr))
}

func (ex *BlankExpr) Type() Type                     { panic("nope") }
func (ex *BlankExpr) ApplyType(typ Type) error       { panic("nope") }
func (ex *BlankExpr) GuessType() (ok bool, typ Type) { panic("nope") }

// Implements convertability definition from Go spec
// https://golang.org/ref/spec#Conversions
func IsConvertable(what TypedExpr, to Type) bool {
	wt := what.Type()

	if IsAssignable(to, wt) {
		return true
	}

	if UnderlyingType(to).String() == UnderlyingType(wt).String() {
		return true
	}

	if to.Kind() == KIND_POINTER && wt.Kind() == KIND_POINTER &&
		UnderlyingType(wt.(*PointerType).To).String() == UnderlyingType(to.(*PointerType).To).String() {
		return true
	}

	// TODO cases:
	// x's type and T are both integer or floating point types.
	// x's type and T are both complex types.
	// x is an integer or a slice of bytes or runes and T is a string type.
	// x is a string and T is a slice of bytes or runes.

	return false
}

// Sometimes it is not immediately obvious if a piece of code is
// an actual expression or a name of a type.
// That can happen during during type conversions, for example in
// the line below we don't know whether 'blah' is a type name or
// a function during parsing:
//
// 	blah(123)
//
// This function tells if an expression is really a type name, and
// returns that type if the answer was yes.
func ExprToTypeName(e Expr) (t Type, ok bool) {
	// TODO: dot operator for packages in below switch:
	switch e := e.(type) {
	case *TypeExpr:
		return e.typ, true
	case *UnaryOp:
		if subType, ok := ExprToTypeName(e.Right); ok {
			return &PointerType{To: subType}, true
		}
	case *Ident:
		if e.object.ObjectType() == OBJECT_TYPE {
			return e.object.(*TypeDecl).Type(), true
		}
	}
	return nil, false
}

func (ex *FuncCallExpr) Type() Type {
	if castType, cast := ExprToTypeName(ex.Left); cast {
		if len(ex.Args) != 1 {
			panic("todo - report error")
		}
		if IsConvertable(ex.Args[0].(TypedExpr), castType) {
			return castType
		}
	} else {
		callee := ex.Left.(TypedExpr)
		calleeType := UnderlyingType(callee.Type())
		if calleeType.Kind() != KIND_FUNC {
			return &UnknownType{}
		}
		asFunc := calleeType.(*FuncType)
		switch {
		case len(asFunc.Results) == 0:
			return &UnknownType{}
		case len(asFunc.Results) == 1:
			return asFunc.Results[0]
		default:
			return &TupleType{Members: asFunc.Results}
		}
	}
	return &UnknownType{}
}

func (ex *FuncCallExpr) ApplyType(typ Type) error {
	if castType, cast := ExprToTypeName(ex.Left); cast {
		if len(ex.Args) != 1 {
			return fmt.Errorf("Type conversion takes exactly one argument")
		}
		// Just try applying, ignore error - even if it fails if might still be convertible.
		ex.Args[0].(TypedExpr).ApplyType(castType)
		if !IsConvertable(ex.Args[0].(TypedExpr), castType) {
			return fmt.Errorf("Impossible conversion from %s to %s", ex.Args[0].(TypedExpr).Type(), castType)
		}
		if !IsAssignable(typ, castType) {
			return fmt.Errorf("Cannot assign `%s` to `%s`", castType, typ)
		}
		return nil
	} else {
		callee := ex.Left.(TypedExpr)
		calleeType := UnderlyingType(callee.Type())
		if calleeType.Kind() != KIND_FUNC {
			return fmt.Errorf("Only functions can be called, not %s", calleeType)
		}

		if typ.Kind() == KIND_TUPLE {
			panic("todo")
		}

		asFunc := calleeType.(*FuncType)
		switch {
		case len(asFunc.Results) == 0:
			return fmt.Errorf("Function `%s` doesn't return anything", asFunc)
		case len(asFunc.Results) == 1:
			if !IsAssignable(asFunc.Results[0], typ) {
				return fmt.Errorf("Can't assign `%s` to `%s`", asFunc.Results[0], typ)
			}
		default:
			return fmt.Errorf("Function `%s` returns more than one result", asFunc)
		}

		for i, arg := range asFunc.Args {
			if err := NegotiateExprType(&arg, ex.Args[i].(TypedExpr)); err != nil {
				return err
			}
		}
		return nil
	}
}

func (ex *FuncCallExpr) GuessType() (ok bool, typ Type) {
	if castType, cast := ExprToTypeName(ex.Left); cast {
		return true, castType
	} else {
		// No guessing needed for now
		return false, nil
	}
}

func (ex *FuncDecl) Type() Type {
	return ex.typ
}
func (ex *FuncDecl) ApplyType(typ Type) error {
	if !IsAssignable(typ, ex.typ) {
		return fmt.Errorf("Cannot assign `%s` to `%s`", ex.typ, typ)
	}
	return ex.Code.CheckTypes()
}
func (ex *FuncDecl) GuessType() (ok bool, typ Type) {
	return false, nil
}

func (cb *CodeBlock) CheckTypes() error {
	for _, stmt := range cb.Statements {
		typedStmt := stmt.(ExprToProcess)
		if err := typedStmt.NegotiateTypes(); err != nil {
			return err
		}
	}
	return nil
}

func (ex *TypeExpr) Type() Type { return ex.typ }
func (ex *TypeExpr) ApplyType(typ Type) error {
	if ex.typ.String() != typ.String() {
		return fmt.Errorf("Different types, %s and %s", ex.typ.String(), typ.String())
	}
	return nil
}
func (ex *TypeExpr) GuessType() (ok bool, typ Type) { return false, nil }

func (ex *DotSelector) Type() Type {
	leftType := ex.Left.(TypedExpr).Type()
	switch leftType.Kind() {
	case KIND_POINTER:
		asPtr := leftType.(*PointerType)
		if asPtr.To.Kind() != KIND_STRUCT {
			return &UnknownType{}
		}
		leftType = asPtr.To
		fallthrough
	case KIND_STRUCT:
		asStruct := leftType.(*StructType)
		member, ok := asStruct.Members[ex.Right.name]
		if !ok {
			// no such member
			return &UnknownType{}
		}
		return member
	case KIND_UNKNOWN:
		panic("todo")
	case KIND_CUSTOM:
		panic("todo")
	default:
		return &UnknownType{}
	}
}

func (ex *DotSelector) ApplyType(typ Type) error {
	if ex.Type().String() != typ.String() {
		return fmt.Errorf("Type %s has no member named %s", ex.Left.(TypedExpr).Type().String(), ex.Right.name)
	}
	return nil
}

func (ex *DotSelector) GuessType() (ok bool, typ Type) {
	return false, nil
}

func (ex *CompoundLit) Type() Type { return nonilTyp(ex.typ) }
func (ex *CompoundLit) ApplyType(typ Type) error {
	var apply = false

	if typ.Kind() == KIND_CUSTOM {
		typ = typ.(*CustomType).RootType()
	}

	switch typ.Kind() {
	case KIND_SLICE:
		asSlice := typ.(*SliceType)

		switch ex.kind {
		case COMPOUND_EMPTY:
			apply = true
		case COMPOUND_LISTLIKE:
			for _, el := range ex.elems {
				if err := el.(TypedExpr).ApplyType(asSlice.Of); err != nil {
					return err
				}
			}
			apply = true
		}
	case KIND_ARRAY:
		asArray := typ.(*ArrayType)

		switch ex.kind {
		case COMPOUND_EMPTY:
			apply = asArray.Size == 0
		case COMPOUND_LISTLIKE:
			if len(ex.elems) == asArray.Size {
				for _, el := range ex.elems {
					if err := el.(TypedExpr).ApplyType(asArray.Of); err != nil {
						return err
					}
				}
				apply = true
			}
		}
	case KIND_STRUCT:
		asStruct := typ.(*StructType)

		switch ex.kind {
		case COMPOUND_EMPTY:
			apply = true
		case COMPOUND_LISTLIKE:
			if len(ex.elems) != len(asStruct.Members) {
				return fmt.Errorf("Type has %d members, but literal has just %d",
					len(asStruct.Members), len(ex.elems))
			}

			for i, el := range ex.elems {
				if err := el.(TypedExpr).ApplyType(asStruct.GetTypeN(i)); err != nil {
					return err
				}
			}
			apply = true
		case COMPOUND_MAPLIKE:
			// TODO: check for duplicates in the literal
			for i := 0; i < len(ex.elems)/2; i++ {
				elName, elType := ex.elems[2*i], ex.elems[2*i+1]

				ident, ok := elName.(*Ident)
				if !ok {
					return fmt.Errorf("Expected a member name")
				}
				name := ident.name
				memb, ok := asStruct.Members[name]
				if !ok {
					return fmt.Errorf("No member named %s", name)
				}
				if err := elType.(TypedExpr).ApplyType(memb); err != nil {
					return err
				}
			}
			apply = true
		}
		//panic("todo")
	case KIND_MAP:
		asMap := typ.(*MapType)

		switch ex.kind {
		case COMPOUND_EMPTY:
			apply = true
		case COMPOUND_MAPLIKE:
			for i, el := range ex.elems {
				if i%2 == 0 {
					if err := el.(TypedExpr).ApplyType(asMap.By); err != nil {
						return err
					}
				} else {
					if err := el.(TypedExpr).ApplyType(asMap.Of); err != nil {
						return err
					}
				}
			}
			apply = true
		}
	}

	if apply {
		ex.typ = typ
		return nil
	}
	return fmt.Errorf("Can't use a compound literal to initialize type %s", typ.String())
}

func (ex *CompoundLit) GuessType() (ok bool, typ Type) {
	switch ex.kind {
	case COMPOUND_EMPTY:
		return false, nil
	case COMPOUND_LISTLIKE:
		var typ Type = nil
		for _, el := range ex.elems {
			ok, t := el.(TypedExpr).GuessType()
			if !ok {
				//return fmt.Errorf("Can't guess the type of the compound literal, because can't guess the type of %#v", el)
				return false, nil
			}
			if typ == nil {
				typ = nonilTyp(t)
			}
			if typ.String() != t.String() {
				return false, nil
			}
		}
		return true, &SliceType{Of: typ}
	case COMPOUND_MAPLIKE:
		var keyType, valueType Type = nil, nil
		for i, el := range ex.elems {
			ok, t := el.(TypedExpr).GuessType()
			if !ok {
				return false, nil
			}

			if i%2 == 0 {
				if keyType == nil {
					keyType = nonilTyp(t)
				}
				if keyType.String() != t.String() {
					return false, nil
				}
			} else {
				if valueType == nil {
					valueType = nonilTyp(t)
				}
				if valueType.String() != t.String() {
					return false, nil
				}
			}
		}
		return true, &MapType{By: keyType, Of: valueType}
	}
	return false, nil
}

func (ex *BinaryOp) Type() Type {
	// for now, assume Left and Right have the same types
	if ex.op.IsCompOp() {
		return &SimpleType{SIMPLE_TYPE_BOOL}
	}
	return ex.Left.(TypedExpr).Type()
}

// Implements the definition of comparable operands from the Go spec.
func AreComparable(t1, t2 Type) bool {
	if t1.Kind() == KIND_UNKNOWN || t2.Kind() == KIND_UNKNOWN {
		// This still might be eventually work after we run GuessType on the
		// parent expression and underlying types will be set.
		return false
	}

	if !IsAssignable(t1, t2) || !IsAssignable(t2, t1) {
		return false
	}

	return true
}

// Implements the definition of ordered operands from the Go spec.
func AreOrdered(t1, t2 Type) bool {
	if !AreComparable(t1, t2) {
		return false
	}

	if t1.Kind() == KIND_SIMPLE && t2.Kind() == KIND_SIMPLE {
		if t1.(*SimpleType).ID != t2.(*SimpleType).ID {
			return false
		}

		switch t1.(*SimpleType).ID {
		case SIMPLE_TYPE_INT, SIMPLE_TYPE_STRING:
			return true
		}
		return false
	}

	// TODO: other cases
	panic("todo")
}

func firstErr(errors ...error) error {
	for _, e := range errors {
		if e != nil {
			return e
		}
	}
	return nil
}

func (ex *BinaryOp) applyTypeForComparisonOp(typ Type) error {
	leftExpr, rightExpr := ex.Left.(TypedExpr), ex.Right.(TypedExpr)

	if typ.Kind() != KIND_SIMPLE || typ.(*SimpleType).ID != SIMPLE_TYPE_BOOL {
		return fmt.Errorf("Comparison operators return bools, not %s", typ)
	}

	t1 := leftExpr.Type()
	if !t1.Known() {
		var ok bool
		ok, t1 = leftExpr.GuessType()
		if !ok {
			return fmt.Errorf("Couldn't infer type of the left operand")
		}
	}

	t2 := rightExpr.Type()
	if !t2.Known() {
		var ok bool
		ok, t2 = rightExpr.GuessType()
		if !ok {
			return fmt.Errorf("Couldn't infer type of the operand on the right")
		}
	}

	if ex.op.IsOrderOp() {
		if !AreOrdered(t1, t2) {
			return fmt.Errorf("Operands of types %s and %s can't be ordered", t1, t2)
		}
	} else {
		if !AreComparable(t1, t2) {
			return fmt.Errorf("Types %s and %s aren't comparable", t1, t2)
		}
	}

	return firstErr(leftExpr.ApplyType(t1), rightExpr.ApplyType(t2))
}

func (ex *BinaryOp) ApplyType(typ Type) error {
	// TODO: Validate concrete operators and types (logical operators only for bools,
	// numeric operators for numeric types, no tuple types, etc).

	if ex.op.IsCompOp() {
		// Comparison operators have different rules and need to be treated separately.
		return ex.applyTypeForComparisonOp(typ)
	}

	if ex.op.IsLogicalOp() {
		if typ.Kind() != KIND_SIMPLE || typ.(*SimpleType).ID != SIMPLE_TYPE_BOOL {
			return fmt.Errorf("Logical operators return bools, not %s", typ)
		}
	}

	leftExpr, rightExpr := ex.Left.(TypedExpr), ex.Right.(TypedExpr)
	if err := leftExpr.ApplyType(typ); err != nil {
		return err
	}
	return rightExpr.ApplyType(typ)
}

func (ex *BinaryOp) GuessType() (ok bool, typ Type) {
	leftOk, leftType := ex.Left.(TypedExpr).GuessType()
	rightOk, rightType := ex.Right.(TypedExpr).GuessType()

	switch {
	case leftOk && rightOk && leftType.String() == rightType.String():
		// The clearest situation - both expressions were able to guess their types
		// and they are the same.
		return true, leftType
	case leftOk:
		err := ex.Right.(TypedExpr).ApplyType(leftType)
		if err == nil {
			return true, leftType
		}
		fallthrough
	case rightOk:
		err := ex.Left.(TypedExpr).ApplyType(rightType)
		if err == nil {
			return true, rightType
		}
		fallthrough
	default:
		return false, nil
	}
}

func (ex *UnaryOp) Type() Type {
	switch right := ex.Right.(TypedExpr); ex.op.Type {
	case TOKEN_PLUS, TOKEN_MINUS, TOKEN_SHR, TOKEN_SHL:
		return right.Type()
	case TOKEN_MUL:
		subType := right.Type()
		if subType.Kind() != KIND_POINTER {
			// underlying type is not a pointer
			return &UnknownType{}
		}
		return subType.(*PointerType).To
	case TOKEN_AMP:
		return &PointerType{To: right.Type()}
	default:
		panic("todo")
	}
}

func (ex *UnaryOp) ApplyType(typ Type) error {
	// TODO: Validate concrete operators and types (logical operators only for bools,
	// numeric operators for numeric types, no tuple types, etc).
	// The way it should be implemented is to reuse as much as possible with BinaryOp.

	switch right := ex.Right.(TypedExpr); ex.op.Type {
	case TOKEN_PLUS, TOKEN_MINUS, TOKEN_SHR, TOKEN_SHL:
		return right.ApplyType(typ)
	case TOKEN_MUL:
		return right.ApplyType(&PointerType{To: typ})
	case TOKEN_AMP:
		typ = UnderlyingType(typ)
		if typ.Kind() != KIND_POINTER {
			return fmt.Errorf("Not a pointer type")
		}
		to := typ.(*PointerType).To
		return right.ApplyType(to)
	default:
		panic("todo")
	}
}

func (ex *UnaryOp) GuessType() (ok bool, typ Type) {
	switch right := ex.Right.(TypedExpr); ex.op.Type {
	case TOKEN_PLUS, TOKEN_MINUS, TOKEN_SHR, TOKEN_SHL:
		//return right.ApplyType(typ)
		return right.GuessType()
	case TOKEN_MUL:
		ok, typ := right.GuessType()
		if !ok {
			return false, nil
		}
		if typ.Kind() != KIND_POINTER {
			return false, nil
		}
		return true, typ.(*PointerType).To
	case TOKEN_AMP:
		ok, typ := right.GuessType()
		if !ok {
			return false, nil
		}
		return true, &PointerType{To: typ}
	default:
		panic("todo")
	}
	//return ex.Right.(TypedExpr).GuessType()
}

func (ex *Ident) Type() Type {
	if ex.object != nil && ex.object.ObjectType() == OBJECT_VAR {
		return ex.object.(*VarDecl).Type
	}
	return nil
}

func (ex *Ident) ApplyType(typ Type) error {
	if ex.object.ObjectType() != OBJECT_VAR {
		return fmt.Errorf("Identifier %s is not a variable", ex.name)
	}

	//if ex.object.(*VarDecl).Type.String() != typ.String() {
	if !IsAssignable(typ, ex.object.(*VarDecl).Type) {
		return fmt.Errorf("Identifier %s is of type %s, can't assign type %s to it", ex.name, ex.object.(*VarDecl).Type, typ)
	}
	return nil
}

func (ex *Ident) GuessType() (ok bool, typ Type) {
	return false, nil
}

func (ex *BasicLit) Type() Type {
	return nonilTyp(ex.typ)
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
		return nil
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
	return other, nil
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

func (t *TupleType) Negotiate(other Type) (Type, error) {
	panic("todo")
}

func (t *FuncType) Negotiate(other Type) (Type, error) {
	panic("todo")
}

func NegotiateTypes(t1, t2 Type) (Type, error) {
	if t1.Known() && t2.Known() {
		// Both types are fully known, no negotiation needed, just
		// check if they are the same.
		if !IsAssignable(t1, t2) {
			return nil, fmt.Errorf("Wanted type %s, but got type %s",
				t1.String(), t2.String())
		}
		return t1, nil
	}

	if t1.Kind() == t2.Kind() {
		if t1.Kind() == KIND_UNKNOWN { // && t2.Kind() == KIND_UNKNOWN
			return nil, fmt.Errorf("Too little information to infer data types")
		}
		// TODO: Maybe Negotiate() for types won't be needed at all?
		//return t1.Negotiate(t2)
		return nil, fmt.Errorf("Too little information to infer data types")
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
