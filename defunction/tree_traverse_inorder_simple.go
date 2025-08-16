package main

import "fmt"

// 1. 简化的、地道的树节点定义
// 使用 *Node 的 nil 指针来表示空树，不再需要 Leaf 结构体和 Tree 接口
type Node struct {
	Value int
	Left  *Node
	Right *Node
}

// 2. 地道的迭代中序遍历
func inorderIterative(root *Node) []int {
	var result []int
	var stack []*Node // 栈里只存放节点指针

	// currentNode 指针用于在树中移动
	currentNode := root

	// 循环条件：当前节点不为 nil，或者栈中还有待处理的节点
	for currentNode != nil || len(stack) > 0 {
		// 阶段一：一路向左，将沿途所有节点压入栈中
		for currentNode != nil {
			stack = append(stack, currentNode)
			currentNode = currentNode.Left
		}

		// 阶段二：无法再向左，从栈中弹出一个节点并访问
		// 弹出
		lastIndex := len(stack) - 1
		poppedNode := stack[lastIndex]
		stack = stack[:lastIndex]

		// 访问
		result = append(result, poppedNode.Value)

		// 转向右子树，开始处理右半部分
		currentNode = poppedNode.Right
	}

	return result
}

func main() {
	// 使用简化的结构体构建树
	sampleTree := &Node{
		Value: 1,
		Left: &Node{
			Value: 2,
			Left:  &Node{Value: 4}, // Left 和 Right 默认为 nil
			Right: &Node{Value: 5},
		},
		Right: &Node{Value: 3},
	}

	result := inorderIterative(sampleTree)
	fmt.Println("Idiomatic Go Inorder Traversal Result:", result)
	// 输出: Idiomatic Go Inorder Traversal Result: [4 2 5 1 3]
}
