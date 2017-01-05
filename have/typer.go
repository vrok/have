// Negotiate and validate types in an AST.
package have

import (
	"fmt"
	"strings"
)

// Generic instantiation key.
type InstKey string

func NewInstKey(g Generic, params []Type) InstKey {
	name, _ := g.Signature()
	strParams := make([]string, 0, len(params))
	for _, p := range params {
		strParams = append(strParams, p.String())
	}
	return InstKey(name + "[" + strings.Join(strParams, ", ") + "]")
}

type TypesContext struct {
	// Stores negotiated types of expression.
	types map[Expr]Type
	// Stores strings that should be used in place of expressions for code generation.
	// Useful e.g. for instantiations of generics.
	goNames map[Expr]string
	// Stores instantiations of generics.
	instantiations map[InstKey]*Instantiation
}

func (tc *TypesContext) SetType(e Expr, typ Type) { tc.types[e] = typ }
func (tc *TypesContext) GetType(e Expr) Type      { return nonilTyp(tc.types[e]) }
func (tc *TypesContext) IsTypeSet(e Expr) bool    { _, ok := tc.types[e]; return ok }

func NewTypesContext() *TypesContext {
	return &TypesContext{
		types:          map[Expr]Type{},
		goNames:        map[Expr]string{},
		instantiations: map[InstKey]*Instantiation{},
	}
}

// Provides a type checking context to typed expressions.
type ExprToProcess interface {
	Expr
	NegotiateTypes(tc *TypesContext) error
}

type TypedExpr interface {
	Expr

	// Infers type of the expression based on facts that are certain - no guessing should
	// happen at this point.
	// Errors returned from Type() are reported as compilation errors.
	// Type() MUSTN'T return GenericType in any case, but the underlying type instead.
	Type(tc *TypesContext) (Type, error)
	// Called by the type checker when it wants this expression to be of a particular type.
	// If all goes well, this should affect the result of further calls to Type().
	// If applying the given type is impossible, appropriate error should be returned (it
	// doesn't always mean the end of negotiation - further calls to Type/ApplyType/GuessType can
	// be expected).
	ApplyType(tc *TypesContext, typ Type) error
	// Tries to guess the most appropriate type for this expression.
	// GuessType(tc *TypesContext) MUSTN'T return GenericType in any case, but the underlying type instead.
	GuessType(tc *TypesContext) (ok bool, typ Type)
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
	if at, ok := t.(DeclaredType); ok {
		return at.RootType()
	}
	//if t.Kind() == KIND_CUSTOM {
	//	return t.(*CustomType).RootType()
	//}
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

func IsIdentincal(to, what Type) bool {
	return to.String() == what.String()
}

// Implements the definition of assignability from the Go spec.
func IsAssignable(to, what Type) bool {
	if IsInterface(to) {
		return Implements(to, what)
	}

	if IsNamed(to) && IsNamed(what) {
		return to.String() == what.String()
	}

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
	case KIND_GENERIC_INST:
		gen, ok := value.(*GenericType)
		if !ok {
			return false
		}
		valueMethods = gen.Struct.Methods
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
			return false
		}
	}

	return true
}

func IsPackage(e TypedExpr) bool {
	ident, isIdent := e.(*Ident)
	return isIdent && ident.object.ObjectType() == OBJECT_PACKAGE
}

func IsBlank(e TypedExpr) bool {
	ident, isIdent := e.(*Ident)
	return isIdent && ident.name == Blank
}

// Given an expression, returns a function referred by it or nil otherwise.
func funcUnderneath(expr Expr) *FuncDecl {
	if oe, ok := expr.(ObjectExpr); ok {
		vr, ok := oe.ReferedObject().(*Variable)
		if !ok {
			return nil
		}

		fd, ok := vr.init.(*FuncDecl)
		if !ok {
			return nil
		}

		return fd
	}

	return nil
}

func (vs *VarStmt) NegotiateTypes(tc *TypesContext) error {
	for _, v := range vs.Vars {
		err := v.NegotiateTypes(tc)
		if err != nil {
			return err
		}
	}
	return nil
}

func (td *ImportStmt) NegotiateTypes(tc *TypesContext) error { return nil }

func (td *TypeDecl) NegotiateTypes(tc *TypesContext) error { return nil }

func (bs *BranchStmt) NegotiateTypes(tc *TypesContext) error { return nil }

func (ls *LabelStmt) NegotiateTypes(tc *TypesContext) error { return nil }

func (ls *GenericFunc) NegotiateTypes(tc *TypesContext) error { return nil }

func (ls *GenericStruct) NegotiateTypes(tc *TypesContext) error { return nil }

func (ws *WhenStmt) NegotiateTypes(tc *TypesContext) error {
	for _, branch := range ws.Branches {
		fail := false
	loop:
		for i, pred := range branch.Predicates {
			switch pred.Kind {
			case WHEN_KIND_IS:
				if !IsIdentincal(pred.Target, ws.Args[i]) {
					fail = true
					break loop
				}
			case WHEN_KIND_IMPLEMENTS:
				_, ok := RootType(pred.Target).(*IfaceType)
				if !ok {
					return ExprErrorf(branch, "Not an interface: %s", pred.Target)
				}

				if !Implements(pred.Target, ws.Args[i]) {
					fail = true
					break loop
				}
			case WHEN_KIND_DEFAULT:
			}
		}

		branch.True = !fail

		if branch.True {
			return branch.Code.CheckTypes(tc)
		}
	}
	return nil
}

func (rs *ReturnStmt) NegotiateTypes(tc *TypesContext) error {
	if rs.Func.Results.countVars() != len(rs.Values) {
		return ExprErrorf(rs, "Different number of return values")
	}

	i, err := 0, error(nil)
	rs.Func.Results.eachPair(func(v *Variable, init Expr) {
		if err != nil {
			return
		}
		err = NegotiateExprType(tc, &v.Type, rs.Values[i].(TypedExpr))
		i++
	})

	return err
}

func (ls *SendStmt) NegotiateTypes(tc *TypesContext) error {
	ltyp, rtyp := Type(&UnknownType{}), Type(&UnknownType{})

	if err := NegotiateExprType(tc, &ltyp, ls.Lhs.(TypedExpr)); err != nil {
		return err
	}

	if err := NegotiateExprType(tc, &rtyp, ls.Rhs.(TypedExpr)); err != nil {
		return err
	}

	lroot := RootType(ltyp)
	if lroot.Kind() != KIND_CHAN {
		return ExprErrorf(ls.Lhs, "Not a chan used for sending")
	}

	channel := lroot.(*ChanType)
	if channel.Dir == CHAN_DIR_RECEIVE {
		return ExprErrorf(ls.Lhs, "Channel is receive-only")
	}
	if !IsAssignable(channel.Of, rtyp) {
		return ExprErrorf(ls.Rhs, "Send value has to be assignable to channel's base type")
	}

	return nil
}

func (ss *StructStmt) NegotiateTypes(tc *TypesContext) error {
	for _, m := range ss.Struct.Methods {
		if err := m.Code.CheckTypes(tc); err != nil {
			return err
		}
	}
	return nil
}

func (is *IfaceStmt) NegotiateTypes(tc *TypesContext) error {
	return nil
}

// This will overwrite the type pointer by varType.
func NegotiateExprType(tc *TypesContext, varType *Type, value TypedExpr) error {
	*varType = nonilTyp(*varType)

	valueTyp, err := value.Type(tc)
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
		ok, guessedType := value.GuessType(tc)
		if !ok || !guessedType.Known() {
			return ExprErrorf(value, "Too little information to infer types")
		}

		typ = guessedType
	}

	*varType = typ

	valueTyp, err = value.Type(tc)
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
			// GuessType(tc *TypesContext) on it, so we still have a chance.
			// Example where this is used: assigning builtin types to the empty interface.
			ok, guessedType := value.GuessType(tc)
			if ok {
				if !IsAssignable(typ, guessedType) {
					return ExprErrorf(value, "Types %s and %s are not assignable", typ, guessedType)
				}
				return value.ApplyType(tc, guessedType)
			}
		}
		return value.ApplyType(tc, typ)
	} else {
		if !IsAssignable(typ, valueTyp) {
			return ExprErrorf(value, "Types %s and %s are not assignable", typ, valueTyp)
		}
		// Run value.ApplyType with value's own type - seems unnecessary,
		// but ApplyType might do some extra checks as side effects.
		return value.ApplyType(tc, valueTyp)
	}
}

func CheckCondition(tc *TypesContext, expr TypedExpr) error {
	var boolTyp Type = &SimpleType{SIMPLE_TYPE_BOOL}

	err := NegotiateExprType(tc, &boolTyp, expr)
	if err != nil {
		return err
	}

	if !IsBoolAssignable(boolTyp) {
		return ExprErrorf(expr, "Error while negotiating types")
	}
	return nil
}

func negotiateScopedVar(tc *TypesContext, scopedVar Stmt) error {
	if scopedVar == nil {
		return nil
	}

	switch scoped := scopedVar.(type) {
	case *VarStmt:
	// ok
	case *AssignStmt:
		if scoped.Token.Type != TOKEN_ASSIGN {
			return CompileErrorf(scoped.Token, "Only `=` assignment allowed in scoped declarations")
		}
	default:
		return ExprErrorf(scopedVar, "Not a var declaration or assignment")
	}

	return scopedVar.(ExprToProcess).NegotiateTypes(tc)
}

func (is *IfStmt) NegotiateTypes(tc *TypesContext) error {
	for _, b := range is.Branches {
		if err := negotiateScopedVar(tc, b.ScopedVar); err != nil {
			return err
		}

		if b.Condition != nil {
			if err := CheckCondition(tc, b.Condition.(TypedExpr)); err != nil {
				return err
			}
		}

		if err := b.Code.CheckTypes(tc); err != nil {
			return err
		}
	}
	return nil
}

func (ss *SwitchStmt) NegotiateTypes(tc *TypesContext) error {
	if err := negotiateScopedVar(tc, ss.ScopedVar); err != nil {
		return err
	}

	var valExpr TypedExpr = &BasicLit{token: &Token{Type: TOKEN_TRUE}}

	typeSwitch, assertion := false, (*TypeAssertion)(nil)

	if ss.Value != nil {
		switch val := ss.Value.(type) {
		case *VarStmt:
			typeSwitch = true
			init := val.Vars[0].Inits[0]
			var ok bool
			assertion, ok = init.(*TypeAssertion)
			if !ok {
				return ExprErrorf(ss.Value, "Variable not initialized with type assertion")
			}
			if !assertion.ForSwitch {
				return ExprErrorf(ss.Value, "Type switch should use v.(type)")
			}
		case *ExprStmt:
			var ok bool
			if assertion, ok = val.Expression.(*TypeAssertion); ok {
				typeSwitch = true
			} else {
				err := val.NegotiateTypes(tc)
				if err != nil {
					return err
				}

				valExpr = val.Expression.(TypedExpr)
			}
		}
	}

	// Determine type of the switch value. It is done independently from values in switch branches.
	valType, err := valExpr.Type(tc)
	if err != nil {
		return err
	}
	if !valType.Known() {
		var ok bool
		ok, valType = valExpr.GuessType(tc)
		if !ok {
			return ExprErrorf(valExpr, "Couldn't determine type of switch expression")
		}
	}
	err = valExpr.ApplyType(tc, valType)
	if err != nil {
		return err
	}

	wasDefault := false
	for i, b := range ss.Branches {
		if len(b.Values) > 0 {
			if typeSwitch {
				if len(b.Values) != 1 {
					return ExprErrorf(b.Values[0], "More than 1 value in a branch of type switch")
				}
				typ, err := ExprToTypeName(tc, b.Values[0])
				if err != nil {
					return err
				}
				if typ == nil {
					return ExprErrorf(b.Values[0], "Not a type name in type switch")
				}

				if b.TypeSwitchVar != nil {
					b.TypeSwitchVar.Type = typ
				}

				if err := CheckTypeAssert(tc, assertion.Left.(TypedExpr), typ); err != nil {
					return err
				}
			} else {
				if ss.Value == nil && len(b.Values) > 1 {
					return ExprErrorf(b.Values[0], "List of values in freeform switch")
				}

				for _, val := range b.Values {
					err := NegotiateExprType(tc, &valType, val.(TypedExpr))
					if err != nil {
						return ExprErrorf(b.Values[0], "Error with switch clause: %s", i+1, err)
					}

					if !AreComparable(tc, valExpr, val.(TypedExpr)) {
						return ExprErrorf(b.Values[0], "Error with switch clause, values are not comparable")
					}
				}
			}
		} else {
			if wasDefault {
				return ExprErrorf(b, "Error - more than one `default` clause")
			}
			wasDefault = true
		}

		err := b.Code.CheckTypes(tc)
		if err != nil {
			return err
		}
	}

	return nil
}

func (p *PassStmt) NegotiateTypes(tc *TypesContext) error {
	return nil
}

func (c *compilerMacro) NegotiateTypes(tc *TypesContext) error {
	c.Active = true
	return nil
}

// For convenience, always returns TupleType, even for one type (chans).
func iteratorType(containerType Type) (*TupleType, error) {
	ct := RootType(containerType)

	switch ct.Kind() {
	case KIND_ARRAY:
		return &TupleType{[]Type{&SimpleType{SIMPLE_TYPE_INT}, ct.(*ArrayType).Of}}, nil
	case KIND_SLICE:
		return &TupleType{[]Type{&SimpleType{SIMPLE_TYPE_INT}, ct.(*SliceType).Of}}, nil
	case KIND_MAP:
		mapType := ct.(*MapType)
		return &TupleType{[]Type{mapType.By, mapType.Of}}, nil
	case KIND_CHAN:
		chanType := ct.(*ChanType)
		if chanType.Dir == CHAN_DIR_SEND {
			return nil, fmt.Errorf("Can't read from %s (%s)", containerType, ct)
		}
		return &TupleType{[]Type{chanType.Of}}, nil
	default:
		return nil, fmt.Errorf("Type %s is not iterable", containerType)
	}
}

func (fs *ForRangeStmt) NegotiateTypes(tc *TypesContext) error {
	seriesTyp, err := fs.Series.(TypedExpr).Type(tc)
	if err != nil {
		return err
	}

	if !seriesTyp.Known() {
		var ok bool
		ok, seriesTyp = fs.Series.(TypedExpr).GuessType(tc)
		if !ok || !seriesTyp.Known() {
			return ExprErrorf(fs.Series, "Couldn't determine the type")
		}
	}

	err = fs.Series.(TypedExpr).ApplyType(tc, seriesTyp)
	if err != nil {
		return err
	}

	iterType, err := iteratorType(seriesTyp)
	if err != nil {
		return ExprErrorf(fs.Series, err.Error())
	}

	if fs.ScopedVars != nil {
		if len(iterType.Members) < len(fs.ScopedVars.Vars) {
			return ExprErrorf(fs.Series, "Wrong number of iterator vars, max %d", len(iterType.Members))
		}

		// All vars are new, just assign them their types.
		for i, v := range fs.ScopedVars.Vars {
			v.Type = iterType.Members[i]
		}
	} else if fs.OutsideVars != nil {
		if len(iterType.Members) < len(fs.OutsideVars) {
			return ExprErrorf(fs.OutsideVars[0], "Wrong number of iterator vars, max %d", len(iterType.Members))
		}

		// TODO: Check if fs.OutsideVars are addressable
		for i, v := range fs.OutsideVars {
			if IsBlank(v.(TypedExpr)) {
				continue
			}
			varType, err := v.(TypedExpr).Type(tc)
			if err != nil {
				return err
			}

			if !IsAssignable(varType, iterType.Members[i]) {
				return ExprErrorf(v, "Can't use %s for iteration, it's not assignable to %s",
					varType, iterType.Members[i])
			}
		}
	} else {
		panic("niemożliwe")
	}

	if err := fs.Code.CheckTypes(tc); err != nil {
		return err
	}

	return nil
}

func (fs *ForStmt) NegotiateTypes(tc *TypesContext) error {
	if err := negotiateScopedVar(tc, fs.ScopedVar); err != nil {
		return err
	}

	if fs.Condition != nil {
		if err := CheckCondition(tc, fs.Condition.(TypedExpr)); err != nil {
			return err
		}
	}

	if fs.RepeatStmt != nil {
		err := fs.RepeatStmt.(ExprToProcess).NegotiateTypes(tc)
		if err != nil {
			return err
		}
	}

	if err := fs.Code.CheckTypes(tc); err != nil {
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
func NegotiateTupleUnpackAssign(tc *TypesContext, onlyFuncCalls bool, lhsTypes []*Type, rhs TypedExpr) error {
	var tuple *TupleType

	switch rhs.(type) {
	case *FuncCallExpr:
		rhsType, err := rhs.Type(tc)
		if err != nil {
			return err
		}
		if rhsType.Kind() != KIND_TUPLE {
			return ExprErrorf(rhs, "Too few values on the right side (function call returns only 1 result)")
		}
		tuple = rhsType.(*TupleType)
	default:
		// In other cases (non-function-calls), tuples aren't returned explicitly - extra
		// boolean is returned only if two variables are in the lhs expression.
		if onlyFuncCalls {
			// Tuples cannot be stored explicitly at the moment.
			return ExprErrorf(rhs, "Too few values")
		}

		leftTyp, err := rhs.Type(tc)
		if err != nil {
			return err
		}

		var ok = true
		if !leftTyp.Known() {
			ok, leftTyp = rhs.GuessType(tc)
		}

		if !ok || !leftTyp.Known() {
			return ExprErrorf(rhs, "Couldn't determine type of the right side of the assignment")
		}

		tuple = &TupleType{Members: []Type{
			leftTyp,
			&SimpleType{SIMPLE_TYPE_BOOL},
		}}

		if err := rhs.ApplyType(tc, tuple); err != nil {
			return err
		}
	}

	for i, t := range lhsTypes {
		typ := firstKnown(*t, tuple.Members[i])
		if typ == nil {
			return ExprErrorf(rhs, "Too little information to infer types")
		}

		if !tuple.Members[i].Known() {
			return ExprErrorf(rhs, "Unknown type in a tuple")
		}

		if (*t).Kind() == KIND_UNKNOWN {
			*t = typ
		} else if !IsAssignable(*t, tuple.Members[i]) {
			return ExprErrorf(rhs, "Types %s and %s aren't assignable", *t, tuple.Members[i])
		}
	}
	return nil
}

func (as *AssignStmt) NegotiateTypes(tc *TypesContext) error {
	if len(as.Lhs) != len(as.Rhs) {
		if len(as.Rhs) == 1 {
			// We might be dealing with tuple unpacking

			types := make([]*Type, len(as.Lhs))
			for i, v := range as.Lhs {
				var typ Type
				var err error

				if IsBlank(v.(TypedExpr)) {
					typ = &UnknownType{}
				} else {
					typ, err = v.(TypedExpr).Type(tc)
					if err != nil {
						return err
					}
				}
				types[i] = &typ
			}

			return NegotiateTupleUnpackAssign(tc, false, types, as.Rhs[0].(TypedExpr))
		} else {
			return ExprErrorf(as, "Different number of items on the left and right hand side")
		}
	}

	for i := range as.Lhs {
		leftExpr := as.Lhs[i].(TypedExpr)

		var leftType Type
		var err error

		if IsBlank(leftExpr) {
			leftType = &UnknownType{}
		} else {
			leftType, err = as.Lhs[i].(TypedExpr).Type(tc)
			if err != nil {
				return err
			}
		}
		err = NegotiateExprType(tc, &leftType, as.Rhs[i].(TypedExpr))
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

func (p *varInitPair) NegotiateTypes(tc *TypesContext) error {
	if p.init == nil {
		p.init = NewBlankExpr()
	}
	return NegotiateExprType(tc, &p.v.Type, p.init.(TypedExpr))
}

func (vd *VarDecl) NegotiateTypes(tc *TypesContext) error {
	if len(vd.Vars) > 1 && len(vd.Inits) == 1 {
		// Mutliple variables initialized with a single function call - we need to unpack a tuple

		types := make([]*Type, len(vd.Vars))
		for i, v := range vd.Vars {
			types[i] = &v.Type
		}

		return NegotiateTupleUnpackAssign(tc, false, types, vd.Inits[0].(TypedExpr))
	}

	var err error
	vd.eachPair(func(v *Variable, init Expr) {
		if err == nil {
			if init == nil {
				init = NewBlankExpr()
			}
			err = NegotiateExprType(tc, &v.Type, init.(TypedExpr))
		}
	})
	return err
}

func (es *ExprStmt) NegotiateTypes(tc *TypesContext) error {
	te := es.Expression.(TypedExpr)
	typ, err := te.Type(tc)
	if err != nil {
		return err
	}
	if !typ.Known() {
		ok, typ_ := te.GuessType(tc)
		if ok {
			typ = typ_
		}
	}
	if !typ.Known() {
		fc, ok := es.Expression.(*FuncCallExpr)
		if ok && fc.IsNullResult(tc) {
			return nil
		}
		return ExprErrorf(es, "Couldn't infer types")
	}

	return es.Expression.(TypedExpr).ApplyType(tc, typ)
}

func (ex *BlankExpr) Type(tc *TypesContext) (Type, error)            { return &UnknownType{}, nil }
func (ex *BlankExpr) ApplyType(tc *TypesContext, typ Type) error     { return nil }
func (ex *BlankExpr) GuessType(tc *TypesContext) (ok bool, typ Type) { return false, nil }

// Implements convertability definition from Go spec
// https://golang.org/ref/spec#Conversions
func IsConvertable(tc *TypesContext, what TypedExpr, to Type) bool {
	wt, err := what.Type(tc)
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
// returns that type if the answer was yes (nil otherwise).
// Additionaly, if it encounters an error it returns it, usually
// they are non-recoverable and can be printed out as compilation errors.
func ExprToTypeName(tc *TypesContext, e Expr) (t Type, err error) {
	switch e := e.(type) {
	case *TypeExpr:
		return e.typ, nil
	case *UnaryOp:
		subType, err := ExprToTypeName(tc, e.Right)
		if err != nil {
			return nil, err
		}
		if subType == nil {
			return nil, nil
		}
		return &PointerType{To: subType}, nil
	case *Ident:
		if e.object == nil {
			return nil, ExprErrorf(e, "Unknown identifier: %s", e.name)
		}
		if e.object.ObjectType() == OBJECT_TYPE {
			return e.object.(*TypeDecl).Type(), nil
		}
	case *DotSelector:
		if IsPackage(e.Left.(TypedExpr)) {
			importStmt := e.Left.(*Ident).object.(*ImportStmt)
			decl := importStmt.pkg.GetType(e.Right.name)
			if decl != nil {
				return &CustomType{Decl: decl, Name: decl.name, Package: importStmt}, nil
			}
		}
	}
	// No error found, but the expression is not a type.
	return nil, nil
}

// Just like ExprToTypeName, but for generics.
func ExprToGeneric(e Expr) (t Generic, err error) {
	switch e := e.(type) {
	case *Ident:
		if e.object == nil {
			return nil, ExprErrorf(e, "Unknown identifier: %s", e.name)
		}
		if e.object.ObjectType() == OBJECT_GENERIC {
			return e.object.(Generic), nil
		}
	case *DotSelector:
		if IsPackage(e.Left.(TypedExpr)) {
			importStmt := e.Left.(*Ident).object.(*ImportStmt)
			obj := importStmt.pkg.GetObject(e.Right.name)
			if obj != nil && obj.ObjectType() == OBJECT_GENERIC {
				return obj.(Generic), nil
			}
		}
	}
	// No error found, but the expression is no a generic.
	return nil, nil
}

func (ex *FuncCallExpr) inferGeneric(tc *TypesContext) (*Variable, string, error) {
	generic, err := ExprToGeneric(ex.Left)
	if err != nil {
		return nil, "", err
	}
	if generic == nil {
		return nil, "", nil
	}
	// Generic function call with indirect generic params
	_, params := generic.Signature()
	genericFn, isFn := generic.(*GenericFunc)
	if !isFn {
		return nil, "", ExprErrorf(ex.Left, "Expression is not a function")
	}

	var argTypes []Type
	genericFn.Func.Args.eachPair(func(v *Variable, init Expr) {
		argTypes = append(argTypes, v.Type)
	})

	gnParams, err := deduceGenericParams(tc, params, argTypes, ex.Args, genericFn.Func.Ellipsis && !ex.Ellipsis)
	if err != nil {
		return nil, "", ExprErrorf(ex, err.Error())
	}

	obj, goName, errors := generic.Instantiate(tc, gnParams...)
	if len(errors) > 0 {
		// TODO: return all errors
		return nil, "", errors[0]
	}

	if obj.ObjectType() != OBJECT_VAR {
		return nil, "", ExprErrorf(ex, "Result of a generic is not a value")
	}

	return obj.(*Variable), goName, nil
}

func (ex *FuncCallExpr) getCalleeType(tc *TypesContext) (Type, error) {
	generic, goName, err := ex.inferGeneric(tc)
	if err != nil {
		return nil, err
	}
	var calleeType Type
	if generic != nil {
		calleeType = generic.Type
		tc.goNames[ex.Left] = goName
		ex.fn = funcUnderneath(&Ident{object: generic})
	} else {
		callee := ex.Left.(TypedExpr)
		calleeType, err = callee.Type(tc)
		if err != nil {
			return nil, err
		}
		ex.fn = funcUnderneath(ex.Left)
	}
	calleeType = UnderlyingType(calleeType)
	return calleeType, nil
}

// Type check function arguments.
func (ex *FuncCallExpr) checkArgs(tc *TypesContext, asFunc *FuncType) error {
	if len(asFunc.Args) != len(ex.Args) || ex.Ellipsis {
		if asFunc.Ellipsis {
			// This function has a variadic argument.
			if len(asFunc.Args) > 1 && len(asFunc.Args)-1 > len(ex.Args) {
				return ExprErrorf(ex, "Too few arguments, at least %d required", len(asFunc.Args)-1)
			}

			lastArgIdx := len(asFunc.Args) - 1
			for i, arg := range ex.Args {
				idx := i
				if i >= len(asFunc.Args) {
					// Variadic arguments from now on, make idx point to the variadic arg declaration.
					idx = lastArgIdx
				}

				if ex.Ellipsis && idx == lastArgIdx {
					// Not only this funcion has a variadic parameter, but we're also expading a slice
					// onto it (e.g. append(x, someSlice...)).
					var slice Type = &SliceType{Of: asFunc.Args[idx]}
					if err := NegotiateExprType(tc, &slice, arg.(TypedExpr)); err != nil {
						return err
					}
					continue
				}

				if err := NegotiateExprType(tc, &asFunc.Args[idx], arg.(TypedExpr)); err != nil {
					return err
				}
			}
			return nil
		} else if len(ex.Args) == 1 {
			types := make([]*Type, len(asFunc.Args))
			for i, v := range asFunc.Args {
				v := v
				types[i] = &v
			}

			return NegotiateTupleUnpackAssign(tc, true, types, ex.Args[0].(TypedExpr))
		}
		return ExprErrorf(ex, "Wrong number of arguments: %d instead of %d", len(ex.Args), len(asFunc.Args))
	} else {
		for i, arg := range asFunc.Args {
			if err := NegotiateExprType(tc, &arg, ex.Args[i].(TypedExpr)); err != nil {
				return err
			}
		}
	}
	return nil
}

func (ex *FuncCallExpr) Type(tc *TypesContext) (Type, error) {
	if tc.IsTypeSet(ex) {
		return tc.GetType(ex), nil
	}

	castType, err := ExprToTypeName(tc, ex.Left)
	if err != nil {
		return nil, err
	}

	if castType != nil {
		if len(ex.Args) != 1 {
			return nil, ExprErrorf(ex, "Type casts take only 1 argument")
		}
		if IsConvertable(tc, ex.Args[0].(TypedExpr), castType) {
			return castType, nil
		}
	} else {
		calleeType, err := ex.getCalleeType(tc)
		if err != nil {
			return nil, err
		}
		if calleeType.Kind() != KIND_FUNC {
			return &UnknownType{}, nil
		}
		asFunc := calleeType.(*FuncType)

		if calleeType.Known() {
			err := ex.checkArgs(tc, asFunc)
			if err != nil {
				return nil, err
			}
		}

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

func (ex *FuncCallExpr) ApplyType(tc *TypesContext, typ Type) error {
	castType, err := ExprToTypeName(tc, ex.Left)
	if err != nil {
		return err
	}

	if castType != nil {
		if len(ex.Args) != 1 {
			return ExprErrorf(ex, "Type conversion takes exactly one argument")
		}
		// Just try applying, ignore error - even if it fails if might still be convertible.
		ex.Args[0].(TypedExpr).ApplyType(tc, castType)
		if !IsConvertable(tc, ex.Args[0].(TypedExpr), castType) {
			typ, _ := ex.Args[0].(TypedExpr).Type(tc)
			return ExprErrorf(ex, "Impossible conversion from %s to %s", typ, castType)
		}
		if !IsAssignable(typ, castType) {
			return ExprErrorf(ex, "Cannot assign `%s` to `%s`", castType, typ)
		}
		tc.SetType(ex, typ)
		return nil
	} else {
		calleeType, err := ex.getCalleeType(tc)
		if err != nil {
			return err
		}
		if calleeType.Kind() != KIND_FUNC {
			return ExprErrorf(ex, "Only functions can be called, not %s", calleeType)
		}

		if typ.Kind() == KIND_TUPLE {
			panic("todo")
		}

		asFunc := calleeType.(*FuncType)
		switch {
		case len(asFunc.Results) == 0:
			return ExprErrorf(ex, "Function `%s` doesn't return anything", asFunc)
		case len(asFunc.Results) == 1:
			if !IsAssignable(asFunc.Results[0], typ) {
				return ExprErrorf(ex, "Can't assign `%s` to `%s`", asFunc.Results[0], typ)
			}
		default:
			return ExprErrorf(ex, "Function `%s` returns more than one result", asFunc)
		}

		err = ex.checkArgs(tc, asFunc)
		if err != nil {
			return err
		}

		tc.SetType(ex, typ)
		return nil
	}
}

func (ex *FuncCallExpr) GuessType(tc *TypesContext) (ok bool, typ Type) {
	castType, err := ExprToTypeName(tc, ex.Left)
	if err != nil {
		return false, nil
	}

	if castType != nil {
		return true, castType
	} else {
		// No guessing needed for now
		return false, nil
	}
}

// True if this function call doesn't return any value.
// Needs to operate on function call that has been already typechecked.
func (ex *FuncCallExpr) IsNullResult(tc *TypesContext) bool {
	calleeType, err := ex.getCalleeType(tc)
	if err != nil {
		return false
	}
	if calleeType.Kind() != KIND_FUNC {
		return false
	}

	asFunc := calleeType.(*FuncType)
	return len(asFunc.Results) == 0
}

func (ex *FuncDecl) Type(tc *TypesContext) (Type, error) {
	return ex.typ, nil
}
func (ex *FuncDecl) ApplyType(tc *TypesContext, typ Type) error {
	if !IsAssignable(typ, ex.typ) {
		return ExprErrorf(ex, "Cannot assign `%s` to `%s`", ex.typ, typ)
	}
	return ex.Code.CheckTypes(tc)
}
func (ex *FuncDecl) GuessType(tc *TypesContext) (ok bool, typ Type) {
	return false, nil
}

func (cb *CodeBlock) CheckTypes(tc *TypesContext) error {
	for _, stmt := range cb.Statements {
		typedStmt := stmt.(ExprToProcess)
		if err := typedStmt.NegotiateTypes(tc); err != nil {
			return err
		}

		if es, ok := stmt.(*ExprStmt); ok {
			_, ok := es.Expression.(*FuncCallExpr)
			if !ok {
				return ExprErrorf(es, "Expression evaluated but not used")
			}
		}
	}
	return nil
}

func (ex *TypeExpr) Type(tc *TypesContext) (Type, error) { return ex.typ, nil }
func (ex *TypeExpr) ApplyType(tc *TypesContext, typ Type) error {
	if ex.typ.String() != typ.String() {
		return ExprErrorf(ex, "Different types, %s and %s", ex.typ.String(), typ.String())
	}
	return nil
}
func (ex *TypeExpr) GuessType(tc *TypesContext) (ok bool, typ Type) { return false, nil }

func (ex *TypeAssertion) Type(tc *TypesContext) (Type, error) {
	if tc.IsTypeSet(ex) {
		return tc.GetType(ex), nil
	}
	if ex.ForSwitch {
		return &UnknownType{}, nil
	}
	return nonilTyp(ex.Right.typ), nil
}
func (ex *TypeAssertion) ApplyType(tc *TypesContext, typ Type) error {
	if ex.ForSwitch {
		return ExprErrorf(ex, "This is only allowed in switch statements")
	}

	if typ.Kind() == KIND_TUPLE {
		tuple := typ.(*TupleType)
		if len(tuple.Members) != 2 {
			ExprErrorf(ex, "Wrong number of elements on left of type assertion (max. 2)")
		}

		if !IsBoolAssignable(tuple.Members[1]) {
			ExprErrorf(ex, "Second value returned from type assertion is bool, bools aren't assignable to %s", tuple.Members[1])
		}

		tc.SetType(ex, typ)
		typ = tuple.Members[0]
	}

	if ex.Right.typ.String() != typ.String() {
		return ExprErrorf(ex, "Different types: %s and %s", typ, ex.Right.typ)
	}

	te := ex.Left.(TypedExpr)

	teType, err := te.Type(tc)
	if err != nil {
		return err
	}

	if !teType.Known() {
		err := te.ApplyType(tc, ex.Right.typ)
		if err != nil {
			ok, guessedTyp := te.GuessType(tc)
			if !ok {
				return err
			}

			err = te.ApplyType(tc, guessedTyp)
			if err != nil {
				return err
			}
		}
	}

	return CheckTypeAssert(tc, te, ex.Right.typ)
}
func (ex *TypeAssertion) GuessType(tc *TypesContext) (ok bool, typ Type) { return false, nil }

// Check if type assertion is sane.
func CheckTypeAssert(tc *TypesContext, src TypedExpr, target Type) error {
	srcType, err := src.Type(tc)
	if err != nil {
		return err
	}

	if !IsInterface(srcType) {
		return ExprErrorf(src, "Invalid type assertion, non-interface `%s` used as a source", srcType)
	}

	if !IsInterface(target) {
		if !Implements(srcType, target) {
			return ExprErrorf(src, "Impossible type assertion: `%s` doesn't implement `%s`",
				target, srcType)
		}
	}
	return nil
}

func (ex *DotSelector) typeFromPkg() (Type, error) {
	importStmt := ex.Left.(*Ident).object.(*ImportStmt)

	member := importStmt.pkg.GetObject(ex.Right.name)
	if member == nil {
		return nil, ExprErrorf(ex.Right, "Package %s doesn't have member %s", importStmt.name, ex.Right.name)
	}
	typ, err := typeOfObject(member, importStmt.name)
	if err != nil {
		return typ, ExprErrorf(ex, err.Error())
	}
	return typ, err
}

func (ex *DotSelector) Type(tc *TypesContext) (Type, error) {
	if IsPackage(ex.Left.(TypedExpr)) {
		return ex.typeFromPkg()
	}

	leftType, err := ex.Left.(TypedExpr).Type(tc)
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
				return nil, ExprErrorf(ex.Right, "No such member: %s", ex.Right.name)
			}

			member, err = method.Type(tc)
			if err != nil {
				return nil, err
			}
		}
		return member, nil
	case KIND_INTERFACE:
		asIface := leftType.(*IfaceType)
		method, ok := asIface.Methods[ex.Right.name]
		if !ok {
			return nil, ExprErrorf(ex.Right, "No such member: %s", ex.Right.name)
		}

		return method.Type(tc)
	case KIND_UNKNOWN:
		panic("todo")
	default:
		if leftType.Known() {
			return nil, ExprErrorf(ex.Left, "Dot selector used for type %s", leftType)
		}
		return &UnknownType{}, nil
	}
}

func (ex *DotSelector) applyTypeForPkgMemb(typ Type) error {
	importStmt := ex.Left.(*Ident).object.(*ImportStmt)

	member, ok := importStmt.pkg.objects[ex.Right.name]
	if !ok {
		return ExprErrorf(ex.Right, "Package %s doesn't have member %s", importStmt.name, ex.Right.name)
	}
	err := applyTypeToObject(member, importStmt.name, typ)
	if err != nil {
		return ExprErrorf(ex, err.Error())
	}
	return nil
}

func (ex *DotSelector) ApplyType(tc *TypesContext, typ Type) error {
	ident, isIdent := ex.Left.(*Ident)
	if isIdent && ident.object.ObjectType() == OBJECT_PACKAGE {
		return ex.applyTypeForPkgMemb(typ)
	}

	exType, err := ex.Type(tc)
	if err != nil {
		return err
	}
	if exType.String() != typ.String() {
		t, _ := ex.Left.(TypedExpr).Type(tc)
		return ExprErrorf(ex.Right, "Type %s has no member named %s", t, ex.Right.name)
	}
	return nil
}

func (ex *DotSelector) GuessType(tc *TypesContext) (ok bool, typ Type) {
	return false, nil
}

func (ex *DotSelector) ReferedObject() Object {
	return ex.Right.ReferedObject()
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

func (ex *ArrayExpr) Type(tc *TypesContext) (Type, error) {
	if tc.IsTypeSet(ex) {
		// Some type was negotiated already.
		return tc.GetType(ex), nil
	}

	generic, err := ExprToGeneric(ex.Left)
	if err != nil {
		return nil, err
	}

	if generic != nil {
		// This isn't a map/array lookup but a generic function.
		var types []Type

		for i, arg := range ex.Index {
			typ, err := ExprToTypeName(tc, arg)
			if err != nil {
				return nil, err
			}
			if typ == nil {
				return nil, ExprErrorf(arg, "Generic parameter #%d is not a type", i)
			}
			types = append(types, typ)
		}
		obj, goName, errors := generic.Instantiate(tc, types...)
		if len(errors) > 0 {
			// TODO: return all errors
			return nil, errors[0]
		}

		if obj.ObjectType() != OBJECT_VAR {
			return nil, ExprErrorf(ex, "Result of a generic is not a value")
		}

		t := obj.(*Variable).Type
		if t.Known() {
			// No guessing possible, so apply it immediately.
			// It might need to be changed in the future if we change the language to be
			// more relaxed.
			tc.SetType(ex, t)
		}
		tc.goNames[ex] = goName
		ex.object = obj
		return t, nil
	}

	if len(ex.Index) != 1 {
		return nil, ExprErrorf(ex, "Index operator takes exactly 1 argument, not %d", len(ex.Index))
	}

	leftType, err := ex.Left.(TypedExpr).Type(tc)
	if err != nil {
		return nil, err
	}
	ok, _, valueType := ex.baseTypesOfContainer(leftType)
	if !ok {
		return &UnknownType{}, nil
	}
	if _, ok := ex.Index[0].(*SliceExpr); ok {
		return &SliceType{Of: valueType}, nil
	}

	return valueType, nil
}

func (ex *ArrayExpr) applyTypeSliceExpr(tc *TypesContext, typ Type) error {
	if len(ex.Index) != 1 {
		return ExprErrorf(ex, "Index operator takes exactly 1 argument, not %d", len(ex.Index))
	}

	sliceExpr := ex.Index[0].(*SliceExpr)

	leftType, err := ex.leftExprType(tc)
	if err != nil {
		return err
	}
	ok, keyType, valueType := ex.baseTypesOfContainer(leftType)

	if !ok {
		return ExprErrorf(ex, "Couldn't infer cotainer type")
	}

	if !IsTypeInt(keyType) {
		t, _ := ex.leftExprType(tc)
		return ExprErrorf(ex, "Type %s doesn't support slice expressions", t)
	}

	err = firstErr(
		sliceExpr.From.(TypedExpr).ApplyType(tc, &SimpleType{SIMPLE_TYPE_INT}),
		sliceExpr.To.(TypedExpr).ApplyType(tc, &SimpleType{SIMPLE_TYPE_INT}),
	)

	// TODO: Handle second ':' and blank expressions on either side of ':'

	if err != nil {
		return err
	}

	// Slice expression always returns slices, even when used for non-slices.
	resultType := &SliceType{Of: valueType}

	if !IsAssignable(typ, resultType) {
		return ExprErrorf(ex, "Types %s and %s are not assignable", resultType, typ)
	}

	return nil
}

func (ex *ArrayExpr) leftExprType(tc *TypesContext) (Type, error) {
	lt, err := ex.Left.(TypedExpr).Type(tc)
	if err != nil {
		return nil, err
	}
	if !lt.Known() {
		var ok bool
		ok, lt = ex.Left.(TypedExpr).GuessType(tc)
		if !ok {
			return &UnknownType{}, nil
		}
	}
	return lt, nil
}

func (ex *ArrayExpr) ApplyType(tc *TypesContext, typ Type) error {
	if tc.IsTypeSet(ex) {
		// Some type was negotiated already.
		t := tc.GetType(ex)

		if !IsIdentincal(t, typ) {
			return ExprErrorf(ex, "Array expression has type %s, not %s", t, typ)
		}
		return nil
	}

	lt, err := ex.leftExprType(tc)
	if err != nil {
		return err
	}

	if !lt.Known() {
		return ExprErrorf(ex, "Coudln't infer container's type")
	}

	if err := ex.Left.(TypedExpr).ApplyType(tc, lt); err != nil {
		return err
	}

	ok, keyTyp, valueTyp := ex.baseTypesOfContainer(lt)
	if !ok {
		return ExprErrorf(ex, "Coudln't infer container's type")
	}

	if len(ex.Index) != 1 {
		return ExprErrorf(ex, "Index operator takes exactly 1 argument, not %d", len(ex.Index))
	}

	if _, ok := ex.Index[0].(*SliceExpr); ok {
		return ex.applyTypeSliceExpr(tc, typ)
	}

	err = ex.Index[0].(TypedExpr).ApplyType(tc, keyTyp)
	if err != nil {
		return err
	}

	vt := typ

	if typ.Kind() == KIND_TUPLE {
		tuple := typ.(*TupleType)
		if len(tuple.Members) != 2 || !IsBoolAssignable(tuple.Members[1]) {
			return ExprErrorf(ex, "Second value is bool")
		}

		if RootType(lt).Kind() != KIND_MAP {
			return ExprErrorf(ex, "Only map index expressions can return extra bool value")
		}

		// Unwrap the tuple
		vt = tuple.Members[0]
	}

	if !IsAssignable(vt, valueTyp) {
		return ExprErrorf(ex, "Type %s cannot be assigned to %s", valueTyp, typ)
	}

	tc.SetType(ex, typ)
	return nil
}

func (ex *ArrayExpr) GuessType(tc *TypesContext) (ok bool, typ Type) {
	ok, typ = ex.Left.(TypedExpr).GuessType(tc)
	if !ok {
		return false, &UnknownType{}
	}

	ok, _, valueType := ex.baseTypesOfContainer(typ)
	if !ok {
		return false, valueType
	}

	if _, ok := ex.Index[0].(*SliceExpr); ok {
		return true, &SliceType{Of: valueType}
	}

	return true, valueType
}

func (ex *ArrayExpr) ReferedObject() Object {
	return ex.object
}

func (ex *CompoundLit) Type(tc *TypesContext) (Type, error) {
	if ex.typ != nil && ex.typ.Known() {
		return ex.typ, nil
	}

	if ex.Left == nil {
		return &UnknownType{}, nil
	}

	typ, err := ExprToTypeName(tc, ex.Left)
	if err != nil {
		return nil, err
	}
	if typ == nil {
		return nil, ExprErrorf(ex, "Non-type on the left of complex literal")
	}

	ex.typ = typ
	return typ, nil
}

func (ex *CompoundLit) ApplyType(tc *TypesContext, typ Type) error {
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
				if err := el.(TypedExpr).ApplyType(tc, asSlice.Of); err != nil {
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
					if err := el.(TypedExpr).ApplyType(tc, asArray.Of); err != nil {
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
				return ExprErrorf(ex, "Type has %d members, but literal has just %d",
					len(asStruct.Members), len(ex.elems))
			}

			for i, el := range ex.elems {
				if err := el.(TypedExpr).ApplyType(tc, asStruct.GetTypeN(i)); err != nil {
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
					return ExprErrorf(elName, "Expected a member name")
				}
				ident.memberName = true
				name := ident.name
				memb, ok := asStruct.Members[name]
				if !ok {
					return ExprErrorf(elName, "No member named %s", name)
				}
				if err := elType.(TypedExpr).ApplyType(tc, memb); err != nil {
					return err
				}
			}
			apply = true
		}
	case KIND_MAP:
		asMap := rootTyp.(*MapType)

		switch ex.kind {
		case COMPOUND_EMPTY:
			apply = true
		case COMPOUND_MAPLIKE:
			for i, el := range ex.elems {
				if i%2 == 0 {
					if err := el.(TypedExpr).ApplyType(tc, asMap.By); err != nil {
						return err
					}
				} else {
					if err := el.(TypedExpr).ApplyType(tc, asMap.Of); err != nil {
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
	return ExprErrorf(ex, "Can't use a compound literal to initialize type %s", typ.String())
}

func (ex *CompoundLit) GuessType(tc *TypesContext) (ok bool, typ Type) {
	switch ex.kind {
	case COMPOUND_EMPTY:
		return false, nil
	case COMPOUND_LISTLIKE:
		var typ Type = nil
		for _, el := range ex.elems {
			ok, t := el.(TypedExpr).GuessType(tc)
			if !ok {
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
			ok, t := el.(TypedExpr).GuessType(tc)
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

func (ex *BinaryOp) Type(tc *TypesContext) (Type, error) {
	if ex.op.IsCompOp() {
		return &SimpleType{SIMPLE_TYPE_BOOL}, nil
	}

	leftTyp, err := ex.Left.(TypedExpr).Type(tc)
	if err != nil {
		return leftTyp, err
	}
	if leftTyp.Known() {
		return leftTyp, nil
	}
	return ex.Right.(TypedExpr).Type(tc)
}

// Function assumes that two expressions were checked for assignability, and their
// can be used to check if the common root type is comparable.
func isRootTypeComparable(t Type) bool {
	switch t.Kind() {
	case KIND_CHAN, KIND_POINTER, KIND_SIMPLE, KIND_CUSTOM:
		return true
	case KIND_ARRAY:
		return isRootTypeComparable(t.(*ArrayType).Of)
	case KIND_GENERIC_INST, KIND_STRUCT:
		var asStruct *StructType
		switch t.Kind() {
		case KIND_GENERIC_INST:
			asStruct = t.(*GenericType).Struct
		case KIND_STRUCT:
			asStruct = t.(*StructType)
		}

		for _, memb := range asStruct.Members {
			if !isRootTypeComparable(memb) {
				return false
			}
		}
		return true
	case KIND_INTERFACE:
		// Assignability should have alrady been checked before calling this function,
		// so we can don't have to check if one side implements the other side.
		return true
	default:
		return false
	}
}

// Implements the definition of comparable operands from the Go spec.
// Panic if e1's or e2's methods return errors - their types need to be negotiated earlier.
func AreComparable(tc *TypesContext, e1, e2 TypedExpr) bool {
	t1, err := e1.Type(tc)
	if err != nil || t1.Kind() == KIND_UNKNOWN {
		panic(fmt.Errorf("Not type-negotiated expression: %s", err))
	}

	t2, err := e2.Type(tc)
	if err != nil || t2.Kind() == KIND_UNKNOWN {
		panic(fmt.Errorf("Not type-negotiated expression: %s", err))
	}

	// Initial requirement from the Go spec.
	if !IsAssignable(t1, t2) && !IsAssignable(t2, t1) {
		return false
	}

	rootT1, rootT2 := RootType(t1), RootType(t2)

	_, isE1Nil := e1.(*NilExpr)
	_, isE2Nil := e2.(*NilExpr)

	switch {
	case isE2Nil && (rootT1.Kind() == KIND_MAP || rootT1.Kind() == KIND_SLICE || rootT1.Kind() == KIND_FUNC):
		return true
	case isE1Nil && (rootT2.Kind() == KIND_MAP || rootT2.Kind() == KIND_SLICE || rootT2.Kind() == KIND_FUNC):
		return true
	case rootT1.String() == rootT2.String():
		return isRootTypeComparable(rootT1)
	case IsInterface(t1):
		return Implements(t1, t2)
	case IsInterface(t2):
		return Implements(t2, t1)
	}

	return false
}

// Implements the definition of ordered operands from the Go spec.
func AreOrdered(t1, t2 Type) bool {
	rootT1, rootT2 := RootType(t1), RootType(t2)

	if rootT1.String() != rootT2.String() {
		return false
	}

	return IsTypeIntKind(rootT1) || IsTypeFloatKind(rootT1) ||
		IsTypeString(rootT1) || IsTypeSimple(rootT1, SIMPLE_TYPE_RUNE)
}

func firstErr(errors ...error) error {
	for _, e := range errors {
		if e != nil {
			return e
		}
	}
	return nil
}

func (ex *BinaryOp) applyTypeForComparisonOp(tc *TypesContext, typ Type) error {
	leftExpr, rightExpr := ex.Left.(TypedExpr), ex.Right.(TypedExpr)

	if !IsBoolAssignable(typ) {
		return ExprErrorf(ex, "Comparison operators return bools, not %s", typ)
	}

	t1, err := leftExpr.Type(tc)
	if err != nil {
		return err
	}
	if !t1.Known() {
		ok, t := leftExpr.GuessType(tc)
		if ok {
			t1 = t
		}
	}

	t2, err := rightExpr.Type(tc)
	if err != nil {
		return err
	}
	if !t2.Known() {
		ok, t := rightExpr.GuessType(tc)
		if ok {
			t2 = t
		}
	}

	switch {
	case t1.Known() && !t2.Known():
		err = rightExpr.ApplyType(tc, t1)
		t2 = t1
	case !t1.Known() && t2.Known():
		err = leftExpr.ApplyType(tc, t2)
		t1 = t2
	case !t1.Known() && !t2.Known():
		err = ExprErrorf(ex, "Couldn't infer types of left and right operands")
	}

	if err != nil {
		return err
	}

	err = firstErr(leftExpr.ApplyType(tc, t1), rightExpr.ApplyType(tc, t2))
	if err != nil {
		return err
	}

	if ex.op.IsOrderOp() {
		if !AreOrdered(t1, t2) {
			return ExprErrorf(ex, "Operands of types %s and %s can't be ordered", t1, t2)
		}
	} else {
		if !AreComparable(tc, leftExpr, rightExpr) {
			return ExprErrorf(ex, "Types %s and %s aren't comparable", t1, t2)
		}
	}

	return nil
}

func (ex *BinaryOp) ApplyType(tc *TypesContext, typ Type) error {
	// TODO: Validate concrete operators and types (logical operators only for bools,
	// numeric operators for numeric types, no tuple types, etc).

	if ex.op.IsCompOp() {
		// Comparison operators have different rules and need to be treated separately.
		return ex.applyTypeForComparisonOp(tc, typ)
	}

	if ex.op.IsLogicalOp() {
		if !IsBoolAssignable(typ) {
			return ExprErrorf(ex, "Logical operators return bools, not %s", typ)
		}
	}

	leftExpr, rightExpr := ex.Left.(TypedExpr), ex.Right.(TypedExpr)
	if err := leftExpr.ApplyType(tc, typ); err != nil {
		return err
	}
	return rightExpr.ApplyType(tc, typ)
}

func (ex *BinaryOp) GuessType(tc *TypesContext) (ok bool, typ Type) {
	leftOk, leftType := ex.Left.(TypedExpr).GuessType(tc)
	rightOk, rightType := ex.Right.(TypedExpr).GuessType(tc)

	if leftOk && rightOk && leftType.String() == rightType.String() {
		// The clearest situation - both expressions were able to guess their types
		// and they are the same.
		return true, leftType
	}
	if leftOk {
		err := ex.Right.(TypedExpr).ApplyType(tc, leftType)
		if err == nil {
			return true, leftType
		}
	}
	if rightOk {
		err := ex.Left.(TypedExpr).ApplyType(tc, rightType)
		if err == nil {
			return true, rightType
		}
	}
	return false, nil
}

func (ex *UnaryOp) Type(tc *TypesContext) (Type, error) {
	if tc.IsTypeSet(ex) {
		// Some type was negotiated already.
		return tc.GetType(ex), nil
	}

	rightType, err := ex.Right.(TypedExpr).Type(tc)
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

func (ex *UnaryOp) ApplyType(tc *TypesContext, typ Type) error {
	// TODO: Validate concrete operators and types (logical operators only for bools,
	// numeric operators for numeric types, no tuple types, etc).
	// The way it should be implemented is to reuse as much as possible with BinaryOp.

	switch right := ex.Right.(TypedExpr); ex.op.Type {
	case TOKEN_PLUS, TOKEN_MINUS, TOKEN_SHR, TOKEN_SHL:
		return right.ApplyType(tc, typ)
	case TOKEN_MUL:
		return right.ApplyType(tc, &PointerType{To: typ})
	case TOKEN_AMP:
		typ = UnderlyingType(typ)
		if typ.Kind() != KIND_POINTER {
			return ExprErrorf(ex, "Not a pointer type")
		}
		to := typ.(*PointerType).To
		return right.ApplyType(tc, to)
	case TOKEN_SEND:
		rightType, err := right.Type(tc)
		if err != nil {
			return err
		}
		rootTyp := RootType(rightType)
		if rootTyp.Kind() != KIND_CHAN {
			return ExprErrorf(ex, "Type %s is not a channel", rightType)
		}
		if rootTyp.(*ChanType).Dir == CHAN_DIR_SEND {
			return ExprErrorf(ex, "Type %s is a send-only channel", rightType)
		}

		if typ.Kind() == KIND_TUPLE {
			tuple := typ.(*TupleType)
			if len(tuple.Members) != 2 {
				ExprErrorf(ex, "Wrong number of elements on channel receive (max. 2)")
			}

			if !IsBoolAssignable(tuple.Members[1]) {
				ExprErrorf(ex, "Second value returned from chan receive is bool, and bools aren't assignable to %s", tuple.Members[1])
			}

			tc.SetType(ex, typ)
			typ = tuple.Members[0]
		}

		if !IsAssignable(rootTyp.(*ChanType).Of, typ) {
			return ExprErrorf(ex, "Types %s and %s are not assignable", rootTyp.(*ChanType).Of, typ)
		}
		return nil
	default:
		panic("todo")
	}
}

func (ex *UnaryOp) GuessType(tc *TypesContext) (ok bool, typ Type) {
	switch right := ex.Right.(TypedExpr); ex.op.Type {
	case TOKEN_PLUS, TOKEN_MINUS, TOKEN_SHR, TOKEN_SHL:
		return right.GuessType(tc)
	case TOKEN_MUL:
		ok, typ := right.GuessType(tc)
		if !ok {
			return false, nil
		}
		if typ.Kind() != KIND_POINTER {
			return false, nil
		}
		return true, typ.(*PointerType).To
	case TOKEN_AMP:
		ok, typ := right.GuessType(tc)
		if !ok {
			return false, nil
		}
		return true, &PointerType{To: typ}
	case TOKEN_SEND:
		ok, typ := right.GuessType(tc)
		if !ok {
			return false, nil
		}
		return true, &ChanType{Of: typ}
	default:
		panic("todo")
	}
}

// Helper function for expressions with common Type() implementations.
func typeOfObject(obj Object, name string) (Type, error) {
	if obj != nil && obj.ObjectType() == OBJECT_VAR {
		return obj.(*Variable).Type, nil
	}
	return nil, fmt.Errorf("Unknown identifier: %s", name)
}

func applyTypeToObject(obj Object, name string, typ Type) error {
	if obj == nil {
		return fmt.Errorf("Unknown identifier: %s", name)
	}

	if obj.ObjectType() != OBJECT_VAR {
		return fmt.Errorf("Identifier %s is not a variable", name)
	}

	if !IsAssignable(typ, obj.(*Variable).Type) {
		return fmt.Errorf("Identifier %s is of type %s, can't assign type %s to it", name, obj.(*Variable).Type, typ)
	}
	return nil
}

func (ex *Ident) Type(tc *TypesContext) (Type, error) {
	typ, err := typeOfObject(ex.object, ex.name)
	if err != nil {
		return typ, ExprErrorf(ex, err.Error())
	}
	return typ, nil
}

func (ex *Ident) ApplyType(tc *TypesContext, typ Type) error {
	err := applyTypeToObject(ex.object, ex.name, typ)
	if err != nil {
		return ExprErrorf(ex, err.Error())
	}
	return nil
}

func (ex *Ident) GuessType(tc *TypesContext) (ok bool, typ Type) {
	return false, nil
}

func (ex *Ident) ReferedObject() Object {
	return ex.object
}

func (ex *NilExpr) Type(tc *TypesContext) (Type, error) {
	return tc.GetType(ex), nil
}

func (ex *NilExpr) ApplyType(tc *TypesContext, typ Type) error {
	switch RootType(typ).Kind() {
	case KIND_POINTER, KIND_INTERFACE, KIND_MAP, KIND_SLICE, KIND_FUNC:
		tc.SetType(ex, typ)
		return nil
	}
	return ExprErrorf(ex, "Type %s can't be set to nil", typ)
}

func (ex *NilExpr) GuessType(tc *TypesContext) (ok bool, typ Type) {
	return false, &UnknownType{}
}

func (ex *BasicLit) Type(tc *TypesContext) (Type, error) {
	return tc.GetType(ex), nil
}

func (ex *BasicLit) ApplyType(tc *TypesContext, typ Type) error {
	actualType := RootType(typ)

	if actualType.Kind() != KIND_SIMPLE {
		return ExprErrorf(ex, "Can't use this literal for type %s", typ)
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

		tc.SetType(ex, typ)
		return nil
	}
	return ExprErrorf(ex, "Can't use this literal for type %s", typ)
}

func (ex *BasicLit) GuessType(tc *TypesContext) (ok bool, typ Type) {
	switch ex.token.Type {
	case TOKEN_STR:
		return true, &SimpleType{ID: SIMPLE_TYPE_STRING}
	case TOKEN_INT:
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

// deduceGenericParams negotitates generic arguments based on types of arguments used in a function call.
// ellipsisExpand is true if the last argument is declared with an ellipsis, but ellipsis wasn't used in
// the function call (so e.g. `append(l, 1, 2)`, but NOT `append(l, []int{1, 2}...)`).
func deduceGenericParams(tc *TypesContext, params []string, decls []Type, uses []Expr, ellipsisExpand bool) ([]Type, error) {
	if ellipsisExpand {
		// Use just the first one for deduction. The rest will be checked for assignability later on.
		uses = uses[:len(decls)]
	}

	if len(decls) != len(uses) {
		// TODO: tuple passing
		return nil, fmt.Errorf("Invalid number of arguments: %d instead of %d", len(uses), len(decls))
	}

	// Requirements for each param inferred from uses. If all goes well, each param
	// should have exacly one requirement.
	reqs := make(map[string]Type, len(params))

	usesTypes := make([]Type, len(uses))
	for i, expr := range uses {
		typ, err := expr.(TypedExpr).Type(tc)
		if err != nil {
			return nil, err
		}
		usesTypes[i] = typ
	}

	var err error

	for _, guessing := range [...]bool{false, true} {
		if len(reqs) == len(params) {
			// We've already deduced all types, no need to guess. This isn't just an optimisation,
			// e.g. for func[T](a, b T) instantiated like this: func(x, 1), where x is float32, guessing
			// the type of `1` would result in an error (T required to be both int and float32).
			break
		}

		for i := range uses {
			decl, use := decls[i], usesTypes[i]

			if !guessing {
				if !use.Known() {
					continue
				}
			} else {
				if use.Known() {
					continue
				}
				var ok bool
				ok, use = uses[i].(TypedExpr).GuessType(tc)
				if !ok {
					return nil, fmt.Errorf("Argument #%d has unknown type", i)
				}
			}

			declSubts := []Type{}
			mapSubtype(decl, func(t Type) bool {
				declSubts = append(declSubts, t)
				return true
			})
			j := 0
			mapSubtype(use, func(t Type) bool {
				if err != nil {
					return false
				}

				declSubt := declSubts[j]
				if declSubt.Kind() == KIND_GENERIC_PARAM {
					name := declSubt.(*GenericParamType).Name
					if req, ok := reqs[name]; ok {
						if req.String() != t.String() {
							err = fmt.Errorf("%s can't be both %s and %s", name, req, t)
							return false
							// ERROR, contradictory requirements
						}
					} else {
						reqs[name] = t
					}
					j++
					return false
				} else if declSubt.Kind() != t.Kind() {
					err = fmt.Errorf("Generic function and the parameter have incomptible types (%s and %s)", declSubt, t)
					return false
				}

				j++
				return true
			})

			if err != nil {
				return nil, err
			}
		}
	}

	// Check if all is known. If not, we'll need to another round with GuessTypes.
	if len(reqs) != len(params) {
		return nil, fmt.Errorf("Not all parameters were guessed")
	}

	result := make([]Type, 0, len(params))
	for _, p := range params {
		result = append(result, reqs[p])
	}
	return result, nil
}
