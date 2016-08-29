package have

// Stack of scopes available to the piece of code that is currently
// being parsed. It is a living stack, scopes are pushed to and popped
// from it as new blocks of code start and end.
// It is used for initial bonding of names and objects (packages,
// variables, types), which later helps the type checker.
type IdentStack []map[string]Object

func (is *IdentStack) erase() {
	*is = IdentStack{}
}

func (is *IdentStack) pushScope() {
	*is = append(*is, map[string]Object{})
}

func (is *IdentStack) popScope() {
	*is = (*is)[:len(*is)-1]
}

func (is *IdentStack) empty() bool {
	return len(*is) == 0
}

func (is *IdentStack) addObject(v Object) {
	if v.Name() == Blank {
		return
	}
	(*is)[len(*is)-1][v.Name()] = v
}

// Returns nil when not found
func (is *IdentStack) findObject(name string) Object {
	if decl, ok := GetBuiltinType(name); ok {
		return decl
	}
	for i := len(*is) - 1; i >= 0; i-- {
		if v, ok := (*is)[i][name]; ok {
			return v
		}
	}
	return nil
}

// Returns either a *TypeDecl or *GenericTypeDecl.(or nil when not found).
func (is *IdentStack) findTypeDecl(name string) Object {
	if decl, ok := GetBuiltinType(name); ok {
		return decl
	}
	for i := len(*is) - 1; i >= 0; i-- {
		if v, ok := (*is)[i][name]; ok && (v.ObjectType() == OBJECT_TYPE || v.ObjectType() == OBJECT_GENERIC_TYPE) {
			return v
		}
	}
	return nil
}
