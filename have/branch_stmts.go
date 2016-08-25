package have

// BranchStmtsMap is used to store branch statements (break, continue, goto) that
// haven't been paired with their labels/statements yet. When parser
// finishes parsing blocks of code/branchable statments, it looks into
// this structure to see if there were any matches.
type BranchStmtsMap map[string][]*BranchStmt

// BranchStmtsTree stores a hierarchy of BranchStmtsMaps. Each nested block of code
// has its own BranchStmtsMap. This tree is needed to detect situations where gotos
// jump into blocks of code (only jumping outside/within blocks is possible).
type BranchStmtsTree struct {
	Members  BranchStmtsMap
	Children []*BranchStmtsTree
}

func NewBranchStmtsTree() *BranchStmtsTree {
	return &BranchStmtsTree{
		Members: BranchStmtsMap{},
	}
}

type BranchTreesStack []*BranchStmtsTree

func (bts *BranchTreesStack) top() *BranchStmtsTree {
	return (*bts)[len(*bts)-1]
}

func (bts *BranchTreesStack) pushNew() *BranchStmtsTree {
	b := bts.top().NewChild()
	*bts = append(*bts, b)
	return b
}

func (bts *BranchTreesStack) pop() {
	*bts = (*bts)[:len(*bts)-1]
}

func (b *BranchStmtsTree) FindAll(label string) []*BranchStmt {
	result := []*BranchStmt{}
	result = append(result, b.Members.FindAll(label)...)
	for _, child := range b.Children {
		result = append(result, child.FindAll(label)...)
	}
	return result
}

func (b *BranchStmtsTree) CountBranchStmts() int {
	sum := len(b.Members)
	for _, child := range b.Children {
		sum += child.CountBranchStmts()
	}
	return sum
}

// Call MatchGotoLabels on every BranchStmtsMap in the tree.
func (b *BranchStmtsTree) MatchGotoLabels(labels map[string]*LabelStmt) {
	b.Members.MatchGotoLabels(labels)
	for _, child := range b.Children {
		child.MatchGotoLabels(labels)
	}
}

// Call MatchBranchableStmt on every BranchStmtsMap in the tree.
func (b *BranchStmtsTree) MatchBranchableStmt(branchable Stmt, lblName string, allowedBranchStmts ...TokenType) {
	b.Members.MatchBranchableStmt(branchable, lblName, allowedBranchStmts...)
	for _, child := range b.Children {
		child.MatchBranchableStmt(branchable, lblName, allowedBranchStmts...)
	}
}

func (b *BranchStmtsTree) NewChild() *BranchStmtsTree {
	r := NewBranchStmtsTree()
	b.Children = append(b.Children, r)
	return r
}

func (b BranchStmtsMap) Add(bs *BranchStmt) {
	lbl := ""
	if bs.Right != nil {
		lbl = bs.Right.name
	}
	b[lbl] = append(b[lbl], bs)
}

func (b BranchStmtsMap) FindAll(label string) []*BranchStmt {
	return b[label]
}

func (b BranchStmtsMap) Remove(bs *BranchStmt) {
	label := bs.Right.name
	for i, x := range b[label] {
		if x == bs {
			b[label] = append(b[label][:i], b[label][i+1:]...)
			if len(b[label]) == 0 {
				delete(b, label)
			}
			return
		}
	}
}

func (b BranchStmtsMap) MatchGotoLabels(labels map[string]*LabelStmt) {
	for labelName, label := range labels {
		matches := b[labelName]
		if len(matches) == 0 {
			continue
		}

		for i := len(matches) - 1; i >= 0; i-- {
			if matches[i].Token.Type == TOKEN_GOTO {
				matches[i].GotoLabel = label
				matches = append(matches[:i], matches[i+1:]...)
			}
		}

		if len(matches) == 0 {
			delete(b, labelName)
		}
	}
}

func (b BranchStmtsMap) MatchBranchableStmt(branchable Stmt, lblName string, allowedBranchStmts ...TokenType) {
	unnamed, ok := b[lblName]
	switch {
	case !ok:
		return
	case len(unnamed) == 0:
		delete(b, lblName)
	}
	for i := len(unnamed) - 1; i >= 0; i-- {
		x := unnamed[i]
		for _, y := range allowedBranchStmts {
			if y == x.Token.Type {
				x.Branchable = branchable
				unnamed = append(unnamed[:i], unnamed[i+1:]...)
			}
		}
	}
	if len(unnamed) > 0 {
		b[lblName] = unnamed
	} else {
		delete(b, lblName)
	}
}
