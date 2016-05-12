package have

type unboundTypes struct {
	current, other map[string][]*CustomType
	ignore, dirty  bool
}

func newUnboundTypes() *unboundTypes {
	return &unboundTypes{
		current: make(map[string][]*CustomType),
		other:   make(map[string][]*CustomType),
	}
}

func (ut *unboundTypes) add(typeName string, typ *CustomType) {
	ut.current[typeName] = append(ut.current[typeName], typ)
}

func (ut *unboundTypes) matchWithStack(s *IdentStack) {
	for k, v := range ut.current {
		decl := s.findTypeDecl(k)
		if decl == nil {
			continue
		}

		for _, typ := range v {
			typ.Decl = decl
		}
		delete(ut.current, k)
	}
}

func (ut *unboundTypes) enableDirtyMode() {
	if ut.dirty {
		panic("BUG: dirty mode on twice")
	}

	ut.dirty = true
	ut.current, ut.other = make(map[string][]*CustomType), ut.current
}

func (ut *unboundTypes) commit() {
	if !ut.dirty {
		panic("BUG: commiting dirty changes in clean mode")
	}

	ut.dirty = false
	for k, v := range ut.other {
		ut.current[k] = append(ut.current[k], v...)
	}
	ut.other = make(map[string][]*CustomType)
}

func (ut *unboundTypes) abandon() {
	if !ut.dirty {
		panic("BUG: abandoning dirty changes in clean mode")
	}

	ut.dirty = false
	ut.current, ut.other = ut.other, make(map[string][]*CustomType)
}
