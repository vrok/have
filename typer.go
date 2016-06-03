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

	Type() (Type, error)
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

func RootType(t Type) Type {
	if t.Kind() == KIND_CUSTOM {
		return t.(*CustomType).RootType()
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

// Can be used to check if a type is an interface. Works with
// interfaces aliased by named types.
func IsInterface(t Type) bool {
	return RootType(t).Kind() == KIND_INTERFACE
}

// Implements the definition of assignability from the Go spec.
func IsAssignable(to, what Type) bool {
	if IsInterface(to) {
		return Implements(to, what)
	}

	if IsNamed(to) && IsNamed(what) {
		return to.String() == what.String()
	}

	// TODO: handle other cases (nils, interfaces, etc.)
	return UnderlyingType(to).String() == UnderlyingType(what).String()
}

// Tells whether value's methods are a subset of iface's methods.
func Implements(iface, value Type) bool {
	i := RootType(iface).(*IfaceType)

	ptr := false
	if value.Kind() == KIND_POINTER {
		value = value.(*PointerType).To
		ptr = true
	}

	var valueMethods map[string]*FuncDecl

	switch value.Kind() {
	case KIND_CUSTOM:
		valueMethods = value.(*CustomType).Decl.Methods
	case KIND_INTERFACE:
		valueMethods = value.(*IfaceType).Methods
	default:
		// Other types can't have methods, but they still can satsifty
		// the empty interface.
		valueMethods = map[string]*FuncDecl{}
	}

	for _, imet := range i.Methods {
		found := false
		for _, met := range valueMethods {
			if met.name != imet.name {
				continue
			}

			if met.PtrReceiver != ptr {
				continue
			}

			if met.typ.String() != imet.typ.String() {
				continue
			}

			found = true
			break
		}

		if !found {
			//return fmt.Errorf("Interface not satisfied, method %s (%s) missing", imet.name, imet.typ)
			return false
		}
	}

	return true
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

func (td *TypeDecl) NegotiateTypes() error { return nil }

func (bs *BranchStmt) NegotiateTypes() error { return nil }

func (ls *LabelStmt) NegotiateTypes() error { return nil }

func (rs *ReturnStmt) NegotiateTypes() error {
	if rs.Func.Results.countVars() != len(rs.Values) {
		return fmt.Errorf("Different number of return values")
	}

	i, err := 0, error(nil)
	rs.Func.Results.eachPair(func(v *Variable, init Expr) {
		if err != nil {
			return
		}
		err = NegotiateExprType(&v.Type, rs.Values[i].(TypedExpr))
		i++
	})

	return err
}

func (ls *SendStmt) NegotiateTypes() error {
	ltyp, rtyp := Type(&UnknownType{}), Type(&UnknownType{})

	if err := NegotiateExprType(&ltyp, ls.Lhs.(TypedExpr)); err != nil {
		return err
	}

	if err := NegotiateExprType(&rtyp, ls.Rhs.(TypedExpr)); err != nil {
		return err
	}

	lroot := RootType(ltyp)
	if lroot.Kind() != KIND_CHAN {
		return fmt.Errorf("Not a chan used for sending")
	}

	channel := lroot.(*ChanType)
	if channel.Dir == CHAN_DIR_RECEIVE {
		return fmt.Errorf("Channel is receive-only")
	}
	if !IsAssignable(channel.Of, rtyp) {
		return fmt.Errorf("Send value has to be assignable to channel's base type")
	}

	return nil
}

func (ss *StructStmt) NegotiateTypes() error {
	for _, m := range ss.Struct.Methods {
		if err := m.Code.CheckTypes(); err != nil {
			return err
		}
	}
	return nil
}

func (is *IfaceStmt) NegotiateTypes() error {
	return nil
}

// This will overwrite the type pointer by varType.
func NegotiateExprType(varType *Type, value TypedExpr) error {
	*varType = nonilTyp(*varType)

	valueTyp, err := value.Type()
	if err != nil {
		return err
	}
	typ := firstKnown(*varType, valueTyp)
	if typ == nil {
		// Try guessing. Literals like "1", or "{1, 2}" can be used
		// to initialize variables of many types (int/double/etc,
		// array/slice/struct), but if type of the variable is
		// not known, we try to guess it (for these examples,
		// it would be "int" and "[]int").
		ok, guessedType := value.GuessType()
		if !ok || !guessedType.Known() {
			return fmt.Errorf("Too little information to infer types")
		}

		typ = guessedType
	}

	*varType = typ

	valueTyp, err = value.Type()
	if err != nil {
		return err
	}
	if !valueTyp.Known() {
		// Don't always run ApplyType for interfaces - lhs and rhs expressions
		// might have different types and that is on purpose.
		if IsInterface(typ) {
			// If we're dealing with interfaces then we don't want to apply that
			// interface type to the value (unless that's explicitly specified).
			// But we don't know the type of the value. But, we still haven't run
			// GuessType() on it, so we still have a chance.
			// Example where this is used: assigning builtin types to the empty interface.
			ok, guessedType := value.GuessType()
			if ok {
				if !IsAssignable(typ, guessedType) {
					return fmt.Errorf("Types %s and %s are not assignable", typ, guessedType)
				}
				return value.ApplyType(guessedType)
			}
		}
		return value.ApplyType(typ)
	} else {
		if !IsAssignable(typ, valueTyp) {
			return fmt.Errorf("Types %s and %s are not assignable", typ, valueTyp)
		}
		// Run value.ApplyType with value's own type - seems unnecessary,
		// but ApplyType might do some extra checks as side effects.
		return value.ApplyType(valueTyp)
	}
}

func CheckCondition(expr TypedExpr) error {
	var boolTyp Type = &SimpleType{SIMPLE_TYPE_BOOL}

	err := NegotiateExprType(&boolTyp, expr)
	if err != nil {
		return err
	}

	if !IsBoolAssignable(boolTyp) {
		return fmt.Errorf("Error while negotiating types")
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

func (ss *SwitchStmt) NegotiateTypes() error {
	if ss.ScopedVar != nil {
		switch scoped := ss.ScopedVar.(type) {
		case *VarStmt:
		// ok
		case *AssignStmt:
			if scoped.Token.Type != TOKEN_ASSIGN {
				return fmt.Errorf("Only `=` assignment allowed in scoped declarations")
			}
		default:
			return fmt.Errorf("Not a var declaration or assignment")
		}

		err := ss.ScopedVar.(ExprToProcess).NegotiateTypes()
		if err != nil {
			return err
		}
	}

	var valType Type = &SimpleType{SIMPLE_TYPE_BOOL}

	typeSwitch, assertion := false, (*TypeAssertion)(nil)

	if ss.Value != nil {
		switch val := ss.Value.(type) {
		case *VarStmt:
			typeSwitch = true
			init := val.Vars[0].Inits[0]
			var ok bool
			assertion, ok = init.(*TypeAssertion)
			if !ok {
				return fmt.Errorf("Variable not initialized with type assertion")
			}
			if !assertion.ForSwitch {
				return fmt.Errorf("Type switch should use v.(type)")
			}
		case *ExprStmt:
			var ok bool
			if assertion, ok = val.Expression.(*TypeAssertion); ok {
				typeSwitch = true
				//typeAssert.Left.Type()

				// HERE, use CheckTypeAssert too
			} else {
				err := val.NegotiateTypes()
				if err != nil {
					return err
				}

				valType, err = val.Expression.(TypedExpr).Type()
				if err != nil {
					return err
				}
			}
		}
	}

	wasDefault := false
	for i, b := range ss.Branches {
		if len(b.Values) > 0 {
			if typeSwitch {
				if len(b.Values) != 1 {
					return fmt.Errorf("More than 1 value in a branch of type switch")
				}
				typ, isTyp := ExprToTypeName(b.Values[0])
				if !isTyp {
					return fmt.Errorf("Not a type name in type switch")
				}

				if b.TypeSwitchVar != nil {
					b.TypeSwitchVar.Type = typ
				}

				if err := CheckTypeAssert(assertion.Left.(TypedExpr), typ); err != nil {
					return err
				}
			} else {
				if ss.Value == nil && len(b.Values) > 1 {
					return fmt.Errorf("List of values in freeform switch")
				}

				for _, val := range b.Values {
					err := NegotiateExprType(&valType, val.(TypedExpr))
					if err != nil {
						return fmt.Errorf("Error with switch clause: %s", i+1, err)
					}

					expType, err := val.(TypedExpr).Type()
					if err != nil {
						return err
					}
					if !AreComparable(valType, expType) {
						return fmt.Errorf("Error with switch clause, %s is not comparable to %s",
							valType, expType)
					}
				}
			}
		} else {
			if wasDefault {
				return fmt.Errorf("Error - more than one `default` clause")
			}
			wasDefault = true
		}

		err := b.Code.CheckTypes()
		if err != nil {
			return err
		}
	}

	return nil
}

func (p *PassStmt) NegotiateTypes() error {
	return nil
}

func (fs *ForStmt) NegotiateTypes() error {
	if fs.ScopedVarDecl != nil {
		err := fs.ScopedVarDecl.NegotiateTypes()
		if err != nil {
			return err
		}
	}

	if fs.Condition != nil {
		if err := CheckCondition(fs.Condition.(TypedExpr)); err != nil {
			return err
		}
	}

	if fs.RepeatStmt != nil {
		err := fs.RepeatStmt.(ExprToProcess).NegotiateTypes()
		if err != nil {
			return err
		}
	}

	if err := fs.Code.CheckTypes(); err != nil {
		return err
	}

	return nil
}

// Helper function useful for situations where an expression returning
// more than one result is assigned to multiple variables.
// In some situations only func calls unpacking works. Type assertions or map
// inclusion testing doesn't work in non-assignment situtations, examples:
//
// func x() int, bool:
//     return someMap[7] // Doesn't work (in Golang as well)
//
// func x(int, bool):
//     pass
// x(someMap[7]) // Doesn't work (in Golang as well)
//
// UseonlyFuncCalls argument to control this.
func NegotiateTupleUnpackAssign(onlyFuncCalls bool, lhsTypes []*Type, rhs TypedExpr) error {

	var tuple *TupleType

	switch rhs.(type) {
	case *FuncCallExpr:
		rhsType, err := rhs.Type()
		if err != nil {
			return err
		}
		if rhsType.Kind() != KIND_TUPLE {
			return fmt.Errorf("Too few values on the right side (function call returns only 1 result)")
		}
		tuple = rhsType.(*TupleType)
	default:
		// In other cases (non-function-calls), tuples aren't returned explicitly - extra
		// boolean is returned only if two variables are in the lhs expression.
		if onlyFuncCalls {
			// Tuples cannot be stored explicitly at the moment.
			return fmt.Errorf("Too few values")
		}

		leftTyp, err := rhs.Type()
		if err != nil {
			return err
		}

		var ok = true
		if !leftTyp.Known() {
			ok, leftTyp = rhs.GuessType()
		}

		if !ok || !leftTyp.Known() {
			return fmt.Errorf("Couldn't determine type of the right side of the assignment")
		}

		tuple = &TupleType{Members: []Type{
			leftTyp,
			&SimpleType{SIMPLE_TYPE_BOOL},
		}}

		if err := rhs.ApplyType(tuple); err != nil {
			return err
		}
	}

	for i, t := range lhsTypes {
		typ := firstKnown(*t, tuple.Members[i])
		if typ == nil {
			return fmt.Errorf("Too little information to infer types")
		}

		if !tuple.Members[i].Known() {
			return fmt.Errorf("Unknown type in a tuple")
		}

		if (*t).Kind() == KIND_UNKNOWN {
			*t = typ
		} else if !IsAssignable(*t, tuple.Members[i]) {
			return fmt.Errorf("Types %s and %s aren't assignable", *t, tuple.Members[i])
		}
	}
	return nil
}

func (as *AssignStmt) NegotiateTypes() error {
	if len(as.Lhs) != len(as.Rhs) {
		if len(as.Rhs) == 1 {
			// We might be dealing with tuple unpacking

			types := make([]*Type, len(as.Lhs))
			for i, v := range as.Lhs {
				typ, err := v.(TypedExpr).Type()
				if err != nil {
					return err
				}
				types[i] = &typ
			}

			return NegotiateTupleUnpackAssign(false, types, as.Rhs[0].(TypedExpr))
		} else {
			return fmt.Errorf("Different number of items on the left and right hand side")
		}
	}

	for i := range as.Lhs {
		leftType, err := as.Lhs[i].(TypedExpr).Type()
		if err != nil {
			return err
		}
		err = NegotiateExprType(&leftType, as.Rhs[i].(TypedExpr))
		if err != nil {
			return err
		}

		// TODO: check addressability, "_" for ==, and if type is numeric for +=, -=,...
	}
	return nil
}

type varInitPair struct {
	v    *Variable
	init Expr
}

func (p *varInitPair) NegotiateTypes() error {
	if p.init == nil {
		p.init = NewBlankExpr()
	}
	return NegotiateExprType(&p.v.Type, p.init.(TypedExpr))
}

func (vd *VarDecl) NegotiateTypes() error {
	if len(vd.Vars) > 1 && len(vd.Inits) == 1 {
		// Mutliple variables initialized with a single function call - we need to unpack a tuple

		types := make([]*Type, len(vd.Vars))
		for i, v := range vd.Vars {
			types[i] = &v.Type
		}

		return NegotiateTupleUnpackAssign(false, types, vd.Inits[0].(TypedExpr))
	}

	var err error
	var depErrs []*needsDepErr
	vd.eachPair(func(v *Variable, init Expr) {
		if err == nil {
			err = (&varInitPair{v: v, init: init}).NegotiateTypes()

			if depErr, ok := err.(*needsDepErr); ok {
				depErrs = append(depErrs, depErr)
				err = nil
			}
		}
	})
	if err != nil {
		return err
	}

	if len(depErrs) > 0 {
		merged := needsDepErr{
			depName: depErrs[0].depName, // Use the first one for now
		}

		for _, nde := range depErrs {
			merged.unchecked = append(merged.unchecked, nde.unchecked...)
		}

		return &merged
	}

	/*
		var progress bool
		for progress {
			before := len(depErrs)
			for i := len(depErrs) - 1; i >= 0; i-- {
				depErr := depErrs[i]
				err = NegotiateExprType(&(depErr.unchecked.v.Type), depErr.unchecked.init.(TypedExpr))

				_, isDepErr := err.(*needsDepErr)

				switch {
				case err == nil:
					// Fixed - VarDecl had an internal dependency.
					depErrs = append(depErrs[:i], depErrs[i+1:]...)
				case isDepErr:
					// Still unresolved, but might be an external dependency.
				default:
					// Some other error, report it.
					return err
				}
			}
			after := len(depErrs)
			progress = after < before
		}
	*/
	return nil
}

func (es *ExprStmt) NegotiateTypes() error {
	uk := Type(&UnknownType{})
	return NegotiateExprType(&uk, es.Expression.(TypedExpr))
}

func (ex *BlankExpr) Type() (Type, error)            { return &UnknownType{}, nil }
func (ex *BlankExpr) ApplyType(typ Type) error       { return nil }
func (ex *BlankExpr) GuessType() (ok bool, typ Type) { return false, nil }

// Implements convertability definition from Go spec
// https://golang.org/ref/spec#Conversions
func IsConvertable(what TypedExpr, to Type) bool {
	wt, err := what.Type()
	if err != nil {
		panic("BUG: Type() errors should be dealt with before IsConvertable")
	}

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

func (ex *FuncCallExpr) Type() (Type, error) {
	if castType, cast := ExprToTypeName(ex.Left); cast {
		if len(ex.Args) != 1 {
			return nil, fmt.Errorf("Type casts take only 1 argument")
		}
		if IsConvertable(ex.Args[0].(TypedExpr), castType) {
			return castType, nil
		}
	} else {
		callee := ex.Left.(TypedExpr)
		calleeType, err := callee.Type()
		if err != nil {
			return nil, err
		}
		calleeType = UnderlyingType(calleeType)
		if calleeType.Kind() != KIND_FUNC {
			return &UnknownType{}, nil
		}
		asFunc := calleeType.(*FuncType)
		switch {
		case len(asFunc.Results) == 0:
			return &UnknownType{}, nil
		case len(asFunc.Results) == 1:
			return asFunc.Results[0], nil
		default:
			return &TupleType{Members: asFunc.Results}, nil
		}
	}
	return &UnknownType{}, nil
}

func (ex *FuncCallExpr) ApplyType(typ Type) error {
	if castType, cast := ExprToTypeName(ex.Left); cast {
		if len(ex.Args) != 1 {
			return fmt.Errorf("Type conversion takes exactly one argument")
		}
		// Just try applying, ignore error - even if it fails if might still be convertible.
		ex.Args[0].(TypedExpr).ApplyType(castType)
		if !IsConvertable(ex.Args[0].(TypedExpr), castType) {
			typ, _ := ex.Args[0].(TypedExpr).Type()
			return fmt.Errorf("Impossible conversion from %s to %s", typ, castType)
		}
		if !IsAssignable(typ, castType) {
			return fmt.Errorf("Cannot assign `%s` to `%s`", castType, typ)
		}
		return nil
	} else {
		callee := ex.Left.(TypedExpr)
		calleeType, err := callee.Type()
		if err != nil {
			return err
		}
		calleeType = UnderlyingType(calleeType)
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

		if len(asFunc.Args) != len(ex.Args) {
			if len(ex.Args) == 1 {
				types := make([]*Type, len(asFunc.Args))
				for i, v := range asFunc.Args {
					//typ := v.(TypedExpr).Type()
					v := v
					types[i] = &v
				}

				return NegotiateTupleUnpackAssign(true, types, ex.Args[0].(TypedExpr))
			}
			return fmt.Errorf("Wrong number of arguments: %d instead of %d", len(ex.Args), len(asFunc.Args))
		} else {
			for i, arg := range asFunc.Args {
				if err := NegotiateExprType(&arg, ex.Args[i].(TypedExpr)); err != nil {
					return err
				}
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

func (ex *FuncDecl) Type() (Type, error) {
	return ex.typ, nil
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

func (ex *TypeExpr) Type() (Type, error) { return ex.typ, nil }
func (ex *TypeExpr) ApplyType(typ Type) error {
	if ex.typ.String() != typ.String() {
		return fmt.Errorf("Different types, %s and %s", ex.typ.String(), typ.String())
	}
	return nil
}
func (ex *TypeExpr) GuessType() (ok bool, typ Type) { return false, nil }

func (ex *TypeAssertion) Type() (Type, error) {
	if ex.typ != nil {
		return ex.typ, nil
	}
	if ex.ForSwitch {
		return &UnknownType{}, nil
	}
	return nonilTyp(ex.Right.typ), nil
}
func (ex *TypeAssertion) ApplyType(typ Type) error {
	if ex.ForSwitch {
		return fmt.Errorf("This is only allowed in switch statements")
	}

	if typ.Kind() == KIND_TUPLE {
		tuple := typ.(*TupleType)
		if len(tuple.Members) != 2 {
			fmt.Errorf("Wrong number of elements on left of type assertion (max. 2)")
		}

		if !IsBoolAssignable(tuple.Members[1]) {
			fmt.Errorf("Second value returned from type assertion is bool, bools aren't assignable to %s", tuple.Members[1])
		}

		ex.typ, typ = typ, tuple.Members[0]
	}

	if ex.Right.typ.String() != typ.String() {
		return fmt.Errorf("Different types: %s and %s", typ, ex.Right.typ)
	}

	te := ex.Left.(TypedExpr)

	teType, err := te.Type()
	if err != nil {
		return err
	}

	if !teType.Known() {
		err := te.ApplyType(ex.Right.typ)
		if err != nil {
			ok, guessedTyp := te.GuessType()
			if !ok {
				return err
			}

			err = te.ApplyType(guessedTyp)
			if err != nil {
				return err
			}
		}
	}

	return CheckTypeAssert(te, ex.Right.typ)
}
func (ex *TypeAssertion) GuessType() (ok bool, typ Type) { return false, nil }

// Check if type assertion is sane.
func CheckTypeAssert(src TypedExpr, target Type) error {
	srcType, err := src.Type()
	if err != nil {
		return err
	}

	if !IsInterface(srcType) {
		return fmt.Errorf("Invalid type assertion, non-interface `%s` used as a source", srcType)
	}

	if !IsInterface(target) {
		if !Implements(srcType, target) {
			return fmt.Errorf("Impossible type assertion: `%s` doesn't implement `%s`",
				target, srcType)
		}
	}
	return nil
}

func (ex *DotSelector) Type() (Type, error) {
	leftType, err := ex.Left.(TypedExpr).Type()
	if err != nil {
		return nil, err
	}

	if leftType.Kind() == KIND_POINTER {
		asPtr := leftType.(*PointerType)
		leftType = asPtr.To
	}

	leftType = RootType(leftType)

	switch leftType.Kind() {
	case KIND_STRUCT:
		asStruct := leftType.(*StructType)
		member, ok := asStruct.Members[ex.Right.name]
		if !ok {
			method, ok := asStruct.Methods[ex.Right.name]
			if !ok {
				return nil, fmt.Errorf("No such member: %s", ex.Right.name)
			}

			member, err = method.Type()
			if err != nil {
				return nil, err
			}
		}
		return member, nil
	case KIND_INTERFACE:
		asIface := leftType.(*IfaceType)
		method, ok := asIface.Methods[ex.Right.name]
		if !ok {
			return nil, fmt.Errorf("No such member: %s", ex.Right.name)
		}

		return method.Type()
	case KIND_UNKNOWN:
		panic("todo")
	default:
		return &UnknownType{}, nil
	}
}

func (ex *DotSelector) ApplyType(typ Type) error {
	exType, err := ex.Type()
	if err != nil {
		return err
	}
	if exType.String() != typ.String() {
		t, _ := ex.Left.(TypedExpr).Type()
		return fmt.Errorf("Type %s has no member named %s", t, ex.Right.name)
	}
	return nil
}

func (ex *DotSelector) GuessType() (ok bool, typ Type) {
	return false, nil
}

func (ex *ArrayExpr) baseTypesOfContainer(containerType Type) (ok bool, key, value Type) {
	switch root := RootType(containerType); root.Kind() {
	case KIND_MAP:
		return true, root.(*MapType).By, root.(*MapType).Of
	case KIND_SLICE:
		return true, &SimpleType{SIMPLE_TYPE_INT}, root.(*SliceType).Of
	case KIND_ARRAY:
		return true, &SimpleType{SIMPLE_TYPE_INT}, root.(*ArrayType).Of
	case KIND_SIMPLE:
		if root.(*SimpleType).ID == SIMPLE_TYPE_STRING {
			return true, &SimpleType{SIMPLE_TYPE_INT}, &SimpleType{SIMPLE_TYPE_BYTE}
		}
		return false, &UnknownType{}, &UnknownType{}

	case KIND_POINTER:
		to := root.(*PointerType).To
		if to.Kind() == KIND_ARRAY {
			// Yep, that works in Golang too
			return true, &SimpleType{SIMPLE_TYPE_INT}, to.(*ArrayType).Of
		}
		return false, &UnknownType{}, &UnknownType{}
	default:
		return false, &UnknownType{}, &UnknownType{}
	}
}

func (ex *ArrayExpr) Type() (Type, error) {
	if ex.typ != nil {
		// Some type was negotiated already.
		return ex.typ, nil
	}

	leftType, err := ex.Left.(TypedExpr).Type()
	if err != nil {
		return nil, err
	}
	ok, _, valueType := ex.baseTypesOfContainer(leftType)
	if !ok {
		return &UnknownType{}, nil
	}
	if _, ok := ex.Index.(*SliceExpr); ok {
		return &SliceType{Of: valueType}, nil
	}

	return valueType, nil
}

func (ex *ArrayExpr) applyTypeSliceExpr(typ Type) error {
	sliceExpr := ex.Index.(*SliceExpr)

	leftType, err := ex.leftExprType()
	if err != nil {
		return err
	}
	ok, keyType, valueType := ex.baseTypesOfContainer(leftType)

	if !ok {
		return fmt.Errorf("Couldn't infer cotainer type")
	}

	if !IsTypeInt(keyType) {
		t, _ := ex.leftExprType()
		return fmt.Errorf("Type %s doesn't support slice expressions", t)
	}

	err = firstErr(
		sliceExpr.From.(TypedExpr).ApplyType(&SimpleType{SIMPLE_TYPE_INT}),
		sliceExpr.To.(TypedExpr).ApplyType(&SimpleType{SIMPLE_TYPE_INT}),
	)

	// TODO: Handle second ':' and blank expressions on either side of ':'

	if err != nil {
		return err
	}

	// Slice expression always returns slices, even when used for non-slices.
	resultType := &SliceType{Of: valueType}

	if !IsAssignable(typ, resultType) {
		return fmt.Errorf("Types %s and %s are not assignable", resultType, typ)
	}

	return nil
}

func (ex *ArrayExpr) leftExprType() (Type, error) {
	lt, err := ex.Left.(TypedExpr).Type()
	if err != nil {
		return nil, err
	}
	if !lt.Known() {
		var ok bool
		ok, lt = ex.Left.(TypedExpr).GuessType()
		if !ok {
			return &UnknownType{}, nil
		}
	}
	return lt, nil
}

func (ex *ArrayExpr) ApplyType(typ Type) error {
	lt, err := ex.leftExprType()
	if err != nil {
		return err
	}

	if !lt.Known() {
		return fmt.Errorf("Coudln't infer container's type")
	}

	if err := ex.Left.(TypedExpr).ApplyType(lt); err != nil {
		return err
	}

	ok, keyTyp, valueTyp := ex.baseTypesOfContainer(lt)
	if !ok {
		return fmt.Errorf("Coudln't infer container's type")
	}

	if _, ok := ex.Index.(*SliceExpr); ok {
		return ex.applyTypeSliceExpr(typ)
	}

	err = ex.Index.(TypedExpr).ApplyType(keyTyp)
	if err != nil {
		return err
	}

	vt := typ

	if typ.Kind() == KIND_TUPLE {
		tuple := typ.(*TupleType)
		if len(tuple.Members) != 2 || !IsBoolAssignable(tuple.Members[1]) {
			return fmt.Errorf("Second value is bool")
		}

		if RootType(lt).Kind() != KIND_MAP {
			return fmt.Errorf("Only map index expressions can return extra bool value")
		}

		// Unwrap the tuple
		vt = tuple.Members[0]
	}

	if !IsAssignable(vt, valueTyp) {
		return fmt.Errorf("Type %s cannot be assigned to %s", valueTyp, typ)
	}

	ex.typ = typ
	return nil
}

func (ex *ArrayExpr) GuessType() (ok bool, typ Type) {
	ok, typ = ex.Left.(TypedExpr).GuessType()
	if !ok {
		return false, &UnknownType{}
	}

	ok, _, valueType := ex.baseTypesOfContainer(typ)
	if !ok {
		return false, valueType
	}

	if _, ok := ex.Index.(*SliceExpr); ok {
		return true, &SliceType{Of: valueType}
	}

	return true, valueType
}

func (ex *CompoundLit) Type() (Type, error) { return nonilTyp(ex.typ), nil }
func (ex *CompoundLit) ApplyType(typ Type) error {
	var apply = false

	rootTyp := RootType(typ)

	switch rootTyp.Kind() {
	case KIND_SLICE:
		asSlice := rootTyp.(*SliceType)

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
		asArray := rootTyp.(*ArrayType)

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
		asStruct := rootTyp.(*StructType)

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
				ident.memberName = true
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
		asMap := rootTyp.(*MapType)

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

func (ex *BinaryOp) Type() (Type, error) {
	// for now, assume Left and Right have the same types
	if ex.op.IsCompOp() {
		return &SimpleType{SIMPLE_TYPE_BOOL}, nil
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

	if !IsBoolAssignable(typ) {
		return fmt.Errorf("Comparison operators return bools, not %s", typ)
	}

	t1, err := leftExpr.Type()
	if err != nil {
		return err
	}
	if !t1.Known() {
		ok, t := leftExpr.GuessType()
		if ok {
			t1 = t
		}
	}

	t2, err := rightExpr.Type()
	if err != nil {
		return err
	}
	if !t2.Known() {
		ok, t := rightExpr.GuessType()
		if ok {
			t2 = t
		}
	}

	switch {
	case t1.Known() && !t2.Known():
		err = rightExpr.ApplyType(t1)
		t2 = t1
	case !t1.Known() && t2.Known():
		err = leftExpr.ApplyType(t2)
		t1 = t2
	case !t1.Known() && !t2.Known():
		err = fmt.Errorf("Couldn't infer types of left and right operands")
	}

	if err != nil {
		return err
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
		if !IsBoolAssignable(typ) {
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

func (ex *UnaryOp) Type() (Type, error) {
	if ex.typ != nil {
		// Some type was negotiated already.
		return ex.typ, nil
	}

	rightType, err := ex.Right.(TypedExpr).Type()
	if err != nil {
		return nil, err
	}

	switch ex.op.Type {
	case TOKEN_PLUS, TOKEN_MINUS, TOKEN_SHR, TOKEN_SHL:
		return rightType, nil
	case TOKEN_MUL:
		if rightType.Kind() != KIND_POINTER {
			// underlying type is not a pointer
			return &UnknownType{}, nil
		}
		return rightType.(*PointerType).To, nil
	case TOKEN_AMP:
		return &PointerType{To: rightType}, nil
	case TOKEN_SEND:
		rootTyp := RootType(rightType)
		if rootTyp.Kind() != KIND_CHAN {
			return &UnknownType{}, nil
		}
		return rootTyp.(*ChanType).Of, nil
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
	case TOKEN_SEND:
		rightType, err := right.Type()
		if err != nil {
			return err
		}
		rootTyp := RootType(rightType)
		if rootTyp.Kind() != KIND_CHAN {
			return fmt.Errorf("Type %s is not a channel", rightType)
		}
		if rootTyp.(*ChanType).Dir == CHAN_DIR_SEND {
			return fmt.Errorf("Type %s is a send-only channel", rightType)
		}

		if typ.Kind() == KIND_TUPLE {
			tuple := typ.(*TupleType)
			if len(tuple.Members) != 2 {
				fmt.Errorf("Wrong number of elements on channel receive (max. 2)")
			}

			if !IsBoolAssignable(tuple.Members[1]) {
				fmt.Errorf("Second value returned from chan receive is bool, and bools aren't assignable to %s", tuple.Members[1])
			}

			ex.typ, typ = typ, tuple.Members[0]
		}

		if !IsAssignable(rootTyp.(*ChanType).Of, typ) {
			return fmt.Errorf("Types %s and %s are not assignable", rootTyp.(*ChanType).Of, typ)
		}
		return nil
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
	case TOKEN_SEND:
		ok, typ := right.GuessType()
		if !ok {
			return false, nil
		}
		return true, &ChanType{Of: typ}
	default:
		panic("todo")
	}
	//return ex.Right.(TypedExpr).GuessType()
}

func (ex *Ident) Type() (Type, error) {
	if ex.object != nil && ex.object.ObjectType() == OBJECT_VAR {
		return ex.object.(*Variable).Type, nil
	}
	return nil, fmt.Errorf("Unknown ident: %s", ex.name)
}

func (ex *Ident) ApplyType(typ Type) error {
	if ex.object.ObjectType() != OBJECT_VAR {
		return fmt.Errorf("Identifier %s is not a variable", ex.name)
	}

	//if ex.object.(*VarDecl).Type.String() != typ.String() {
	if !IsAssignable(typ, ex.object.(*Variable).Type) {
		return fmt.Errorf("Identifier %s is of type %s, can't assign type %s to it", ex.name, ex.object.(*Variable).Type, typ)
	}
	return nil
}

func (ex *Ident) GuessType() (ok bool, typ Type) {
	return false, nil
}

func (ex *NilExpr) Type() (Type, error) {
	return nonilTyp(ex.typ), nil
}

func (ex *NilExpr) ApplyType(typ Type) error {
	switch RootType(typ).Kind() {
	case KIND_POINTER, KIND_INTERFACE, KIND_MAP, KIND_SLICE, KIND_FUNC:
		ex.typ = typ
		return nil
	}
	return fmt.Errorf("Type %s can't be set to nil", typ)
}

func (ex *NilExpr) GuessType() (ok bool, typ Type) {
	return false, &UnknownType{}
}

func (ex *BasicLit) Type() (Type, error) {
	return nonilTyp(ex.typ), nil
}

func (ex *BasicLit) ApplyType(typ Type) error {
	actualType := RootType(typ)

	if actualType.Kind() != KIND_SIMPLE {
		return fmt.Errorf("Can't use this literal for type %s", typ)
	}

	switch {
	case ex.token.Type == TOKEN_STR &&
		actualType.(*SimpleType).ID == SIMPLE_TYPE_STRING:
		fallthrough
	case ex.token.Type == TOKEN_INT && IsTypeNumeric(actualType):
		fallthrough
	case ex.token.Type == TOKEN_RUNE && IsTypeNumeric(actualType):
		fallthrough
	case ex.token.Type == TOKEN_FLOAT && (IsTypeFloatKind(actualType) || IsTypeComplexType(actualType)):
		fallthrough
	case ex.token.Type == TOKEN_IMAG && IsTypeComplexType(actualType):
		fallthrough
	case (ex.token.Type == TOKEN_TRUE || ex.token.Type == TOKEN_FALSE) &&
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
	case TOKEN_INT:
		// TODO: handle anything else than just integers
		return true, &SimpleType{ID: SIMPLE_TYPE_INT}
	case TOKEN_FLOAT:
		return true, &SimpleType{ID: SIMPLE_TYPE_FLOAT64}
	case TOKEN_IMAG:
		return true, &SimpleType{ID: SIMPLE_TYPE_COMPLEX128}
	case TOKEN_TRUE, TOKEN_FALSE:
		return true, &SimpleType{ID: SIMPLE_TYPE_BOOL}
	case TOKEN_RUNE:
		return true, &SimpleType{ID: SIMPLE_TYPE_RUNE}
	}
	return false, nil
}

func firstKnown(types ...Type) Type {
	for _, t := range types {
		if t.Known() {
			return t
		}
	}

	return nil
}
