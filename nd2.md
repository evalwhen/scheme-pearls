# recur 延迟递归原语
一下 recur 框架定义了如何对一个普通的 list 进行递归, 将递归调用以及计算上下文封装成 thunk 传给用户 callback,
callback 里面可以决定接下来如何调用 thunk。
## 与传统递归比如如下

| **递归模式**       | **传统递归**               | **延迟递归原语**               |
|--------------------|---------------------------|-------------------------------|
| **控制方式**       | 隐式调用栈                | 显式 thunk 传递               |
| **递归触发**       | 自动                      | 处理器函数显式控制            |
| **中间状态**       | 不可访问                  | 完整上下文传递                |
| **适用场景**       | 简单递归                  | 复杂效果（非确定/异步/增量）  |
| **代数保证**       | 依赖语言实现              | 显式等式约束                  |
| **优化潜力**       | 编译器自动优化            | 用户直接控制优化点            |



``` scheme
;; f: h -> t -> c -> (list (list a))
(define (recur f z lst)
  (if (null? lst)
      z  ; 空列表返回基值
      (let ((h (car lst))
            (t (cdr lst)))
        ;; 直接调用 f，传入所有参数
        (f h t (lambda () (recur f z t))))))

(define (nd-insert x nd-lst)
  (nd-recur
   (lambda (h t r-thunk) ;; callback
     (nd-choice
      (nd-cons x (nd-list (cons h t)))  ; 前端插入
      (nd-cons h (r-thunk))))           ; 尾部递归插入
   (nd-list (list x))  ; 基值: [x] (修正为正确的非确定性列表)
   nd-lst))


```

`nd-recur` 原语的设计体现了函数式编程中递归模式的本质，可以概括为以下通用抽象：

### 核心概念：**延迟递归原语 (Lazy Recursion Primitive)**
`nd-recur` 是一种特殊的递归组合子，它：
1. **分离递归定义与触发**：通过 thunk 延迟递归调用
2. **显式控制递归流**：在处理器函数中决定何时展开递归
3. **结构化分解输入**：强制按特定模式（头/尾）处理数据

### 四大核心特征

#### 1. **结构化递归约束**
```scheme
(if (null? choice)
    z  ; 空结构处理
    (let ((h (car choice)) ; 强制分解头部
         (t (cdr choice))) ; 和尾部
```
- **强制按特定结构分解**：输入必须符合递归模式
- **保证终止性**：空结构作为递归基

#### 2. **延迟计算机制**
```scheme
(lambda () (nd-recur f z (list t))) ; thunk 包装递归
```
- **显式延迟**：递归点作为一等公民传递
- **按需触发**：处理器函数决定何时调用 `(r-thunk)`

#### 3. **递归上下文传播**
```scheme
(f h t (lambda () ...)) ; 传入当前元素、尾部和递归thunk
```
- **完整上下文传递**：包含当前数据和递归能力
- **递归隔离**：每个递归路径独立

#### 4. **代数语义保证**
必须满足核心等式：
```
recur c n ∅ ≡ n
recur c n (h⊕t) ≡ c h t (λ_ → recur c n t)
```
其中：
- `∅` 表示空结构
- `⊕` 表示结构分解操作
- `λ_ →` 表示延迟计算

### 四、通用模式实现框架

#### 1. 列表递归模式
```python
def list_recur(f, z, lst):
    if not lst:
        return z
    else:
        h, t = lst[0], lst[1:]
        return f(h, t, lambda: list_recur(f, z, t))
```

#### 2. 二叉树递归模式
```python
def tree_recur(f, z, tree):
    if tree is None:
        return z
    else:
        return f(tree.value, 
                 lambda: tree_recur(f, z, tree.left), 
                 lambda: tree_recur(f, z, tree.right))
```

#### 3. 带状态递归模式
```haskell
state_recur :: (s -> a -> (s -> b) -> s -> b)
            -> b
            -> [a]
            -> s
            -> (b, s)
state_recur f z [] s = (z, s)
state_recur f z (x:xs) s = 
    f s x (\s' -> state_recur f z xs s') s
```

### 五、应用场景扩展

#### 1. 非确定性计算
```haskell
type NonDet a = [a]  -- 选择列表

nd_recur :: (a -> [a] -> (() -> NonDet b) -> NonDet b)
         -> NonDet b
         -> NonDet a
         -> NonDet b
```

#### 2. 异步递归
```typescript
type Async<T> = Promise<T>

async function async_recur<T, R>(
  f: (head: T, tail: T[], recur: () => Async<R>) => Async<R>,
  z: Async<R>,
  list: T[]
): Async<R> {
  if (list.length === 0) return z;
  const [h, ...t] = list;
  return f(h, t, () => async_recur(f, z, t));
}
```

#### 3. 增量计算
```rust
trait Incremental {
    type Output;
    fn compute(&mut self) -> Self::Output;
}

fn inc_recur<T, R, F>(
    f: F,
    z: R,
    mut input: impl Iterator<Item = T>,
) -> impl Incremental<Output = R>
where
    F: Fn(T, &mut dyn FnMut() -> R) -> R,
{
    move || match input.next() {
        Some(h) => f(h, &mut || inc_recur(f, z, input).compute()),
        None => z,
    }
}
```

### 六、理论意义：递归模式分类

### 七、实现原则
1. **结构化分解**：输入数据必须可分解为 `(head, tail)`
2. **延迟递归点**：递归路径必须通过 thunk 传递
3. **上下文完整性**：处理器接收当前数据和递归能力
4. **代数合规**：必须满足空结构和递归分解等式

这种模式已被证明在以下领域高效：
- **非确定性计算**（如论文案例）
- **增量计算**（React 的 Hooks 机制）
- **异步流处理**（RxJS 的递归操作符）
- **依赖解析**（如 Makefile 规则求值）

> 正如论文所示，这种递归原语超越了传统的 `fold`/`reduce`，通过**显式延迟**和**上下文传递**，为复杂效果处理提供了类型安全且优雅的解决方案。
