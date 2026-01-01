package main

import "fmt"

// ==========================================
// 1. 定义 Continuations 的形状 (栈帧)
// ==========================================

// 定义 Continuation 的类型 (Sum Type)
type ContType int

const (
	EndCont  ContType = iota
	Fib1Cont          // 对应 lambda(v1)
	Fib2Cont          // 对应 lambda(v2)
)

// 定义栈帧结构 (Product Type)
// 这是去函数化后的闭包，保存了恢复计算所需的所有上下文
// 栈帧就是闭包的环境部分 (Stack Frame is the Environment of the Closure)。
type StackFrame struct {
	Type ContType
	N    int // Fib1Cont 需要保存 n，以便计算 n-2
	V1   int // Fib2Cont 需要保存 v1，以便计算 v1 + v2
}

func main() {
	n := 10
	result := FibRegisterMachine(n)
	fmt.Printf("Fib(%d) = %d\n", n, result)
}

// ==========================================
// 2. 核心引擎：寄存器机器 (Register Machine)
// ==========================================
// (define (fib-cps n k)
//   (if (< n 2)
//       (k n) ;; Base Case: 直接把 n 给 k
//       ;; Step 1: 先算 n-1
//       (fib-cps (- n 1)
//                ;; Continuation 1: 等着 v1 (即 fib(n-1) 的结果)
//                (lambda (v1)
//                  ;; Step 2: 拿到 v1 后，算 n-2
//                  (fib-cps (- n 2)
//                           ;; Continuation 2: 等着 v2 (即 fib(n-2) 的结果)
//                           (lambda (v2)
//                             ;; Step 3: 拿到 v2 后，相加，传给原来的 k
//                             (k (+ v1 v2))))))))
func FibRegisterMachine(inputN int) int {
	// --- 寄存器 (Registers) ---
	n := inputN            // 参数寄存器
	val := 0               // 返回值寄存器 (Accumulator)
	stack := []StackFrame{ // Continuation 寄存器 (栈)
		{Type: EndCont},   // 初始 Continuation
	}

	// --- 蹦床 (Trampoline Loop) ---
	// 这个循环不断地在 "ApplyFunction" 和 "ApplyContinuation" 之间跳跃
	for {
		// === 相当于 (define (fib n) ...) 的前半部分 ===
		// 每次循环开始，我们都在试图解决当前的 n
		if n < 2 {
			// Base Case: val = n
			//
			// 以下两句对应（k n)
			val = n
			goto ApplyContinuation
		} else {
			// Recursive Step: 我们需要先算 fib(n-1)
			// 对应代码: (fib-cps (- n 1) (lambda (v1) ...))

			// 新建栈帧并入栈
			// 需要保存自由变量 n
			frame := StackFrame{Type: Fib1Cont, N: n}
			stack = append(stack, frame)

			// 2. 更新参数寄存器
			n = n - 1

			// 3. 这里的 continue 相当于 goto LoopStart (递归调用 fib)
			// 由于是尾部递归，因此相当于循环，之间跳转到循环开头
			continue
		}

	ApplyContinuation:
		// === 相当于 (apply-cont k val) ===
		// 此时 val 寄存器里已经有了刚刚计算出的结果
		// 我们查看栈顶，决定下一步做什么

		// 1. Pop 栈顶
		if len(stack) == 0 {
			break // 极其异常的情况
		}
		top := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		// 2. 根据 Continuation 类型分发 (还原闭包行为)
		switch top.Type {

		case EndCont:
			// 栈空了，计算结束，val 就是最终结果
			return val

		case Fib1Cont:
			// 刚从 fib(n-1) 回来。现在的 val 就是 v1。
			// 对应: (lambda (v1) (fib-cps (- n 2) (lambda (v2) ...)))

			v1 := val // 暂存 v1
			savedN := top.N

			// 1. 构造新的 Continuation (Fib2Cont)，保存 v1
			newFrame := StackFrame{Type: Fib2Cont, V1: v1}
			stack = append(stack, newFrame)

			// 2. 准备调用 fib(n-2)
			n = savedN - 2

			// 3. 跳回循环顶部执行 fib(n)
			continue

		case Fib2Cont:
			// 对应: (lambda (v2) (k (+ v1 v2)))

			v2 := val
			v1 := top.V1

			// 计算加法
			sum := v1 + v2

			// 更新 val
			val = sum

			// 现在我们算完了当前层，需要继续"返回"给更外层的 k
			// 所以我们 goto ApplyContinuation (即继续 Pop 下一个栈帧)
			goto ApplyContinuation
		}
	}
	return val
}
