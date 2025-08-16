package main

import "fmt"

// 树的定义，暂时不使用泛型，只包含整数
// 用 interface 模拟几何类型
type Tree interface{
	 isTree()
}

type Leaf struct {}
type Node struct {
	Value int
	Left Tree
	Right Tree
}

func (l *Leaf) isTree() {}
func (n *Node) isTree() {}

//  Continuation
type Continuation interface {
	isCont()
}

type Halt struct{}
type AfterLeft struct {
	Tree *Node
	NextK *Continuation
}
type AfterRight struct {
	Tree *Node
	NextK *Continuation
	LeftValues []int
}

func (h *Halt) isCont() {}
func (h *AfterLeft) isCont() {}
func (h *AfterRight) isCont() {}

// WorkingItem
type WorkingItem interface {
	isWorkingItem()
}

type HandleTree struct {
	Tree Tree
}

type HandleValue struct {
	Value []int
}

// TODO: 看下为什么这样不行
// func (ht *HandleTree) isWorkingItem(){}
func (ht *HandleTree) isWorkingItem(){}
func (ht *HandleValue) isWorkingItem(){}

func TreeTraverseInorder(tree Tree) []int {
	workingItem := WorkingItem(&HandleTree{Tree: tree})
	stacks := []Continuation{&Halt{}}

	for {
		switch item := workingItem.(type) {
		case *HandleTree:
			println("tree")
			switch treeNode := item.Tree.(type) {
			case *Leaf:
				workingItem = &HandleValue{Value: []int{}}
			case *Node:
				workingItem = &HandleTree{Tree: treeNode.Left}
				stacks = append(stacks, &AfterLeft{Tree: treeNode, NextK: &stacks[len(stacks)-1]})
			}
		case *HandleValue:

			top := stacks[len(stacks)-1]
			switch cont := top.(type) {
			case *Halt:
				return item.Value
			case *AfterLeft:
				workingItem = &HandleTree{Tree: cont.Tree.Right}
				stacks = append(stacks, &AfterRight{Tree: cont.Tree, NextK: cont.NextK, LeftValues: item.Value})
			case *AfterRight:
				vs := append(cont.LeftValues, cont.Tree.Value)
				vs = append(vs, item.Value...)
				workingItem = &HandleValue{Value: vs}
				stacks = stacks[:len(stacks) - 2]
				stacks = append(stacks, *cont.NextK)
			}
		}
	}
}

func main() {
	// 构建您在 Scheme 中定义的示例树
	//     1
	//    / \
	//   2   3
	//  / \
	// 4   5
	sampleTree := &Node{
		Value: 1,
		Left: &Node{
			Value: 2,
			Left:  &Node{Value: 4, Left: &Leaf{}, Right: &Leaf{}},
			Right: &Node{Value: 5, Left: &Leaf{}, Right: &Leaf{}},
		},
		Right: &Node{Value: 3, Left: &Leaf{}, Right: &Leaf{}},
	}

	result := TreeTraverseInorder(sampleTree)
	fmt.Println("Go for-loop Inorder Traversal Result:", result)
	// 输出: Go for-loop Inorder Traversal Result: [4 2 5 1 3]
}
