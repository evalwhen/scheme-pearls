(* 'a tree 是一个多态类型，可以存放任意类型的值 *)
type 'a tree =
  | Empty (* 代表空树，相当于 Scheme 的 null? 或 '() *)
  | Node of 'a * 'a tree * 'a tree (* 代表一个节点，包含一个值、左子树和右子树 *)

(* 版本一递归遍历 *)
let rec preorder_rec (t : 'a tree) : 'a list =
  match t with
  | Empty -> [] (* 基准情况：空树返回空列表 *)
  | Node (v, left, right) ->
      (* 递归情况：根节点值 :: (左子树列表 @ 右子树列表) *)
      v :: (preorder_rec left @ preorder_rec right)

(* 版本二将递归转化成 cps *)
let rec preorder_cps (t : 'a tree) (k : 'a list -> 'b) : 'b =
  match t with
  | Empty -> k [] (* 基准情况：将空列表传递给续延 k *)
  | Node (v, left, right) ->
      (* 递归情况：*)
      (* 1. 计算左子树，其续延是“拿到左列表后去计算右子树” *)
      preorder_cps left (fun left_val ->
        (* 2. 计算右子树，其续延是“拿到右列表后去合并所有结果” *)
        preorder_cps right (fun right_val ->
          (* 3. 所有结果都已拿到，执行最终合并，并把结果传递给最初的续延 k *)
          k (v :: (left_val @ right_val))
        )
      )


(* 版本三将 continuation 转化成数据结构 *)
(* 定义数据化的续延类型 *)
type 'a cont =
  | Halt (* 结束标志 *)
  | HandleRight of 'a tree * 'a cont (* 处理完左子树，需要记住父节点和下一层续延 *)
  | DoAppend of 'a * 'a list * 'a cont (* 处理完右子树，需要记住父节点值、左列表和下一层续延 *)

let rec worker_rec (t : 'a tree) (k : 'a cont) : 'a list =
  match t with
  | Empty -> apply_k_rec k [] (* 空树，将 '() "返回"给续延 *)
  | Node (v, left, right) ->
      (* 深入左子树，并将当前工作（处理右子树）压入续延栈 *)
      worker_rec left (HandleRight (t, k))

and apply_k_rec (k : 'a cont) (v : 'a list) : 'a list =
  match k with
  | Halt -> v (* 续延栈为空，v 就是最终结果 *)
  | HandleRight (Node (node_v, _, node_r), k_rest) ->
      (* 左子树结果是 v，现在去处理右子树 *)
      worker_rec node_r (DoAppend (node_v, v, k_rest))
  | HandleRight (Empty, _) -> failwith "Invalid state: HandleRight received Empty tree"
  | DoAppend (node_v, left_val, k_rest) ->
      (* 右子树结果是 v，现在合并所有结果 *)
      apply_k_rec k_rest (node_v :: (left_val @ v))


(* 版本四通过 trampoline 取消相互递归 *)
(* 续延类型和版本 3 相同 *)
type 'a cont =
  | Halt
  | HandleRight of 'a tree * 'a cont
  | DoAppend of 'a * 'a list * 'a cont

(* 指令类型，用于蹦床和 worker/apply_k 之间的通信 *)
type 'a command =
  | Compute of 'a tree * 'a cont
  | Done of 'a list

(* apply_k 现在返回指令，而不是直接递归 *)
let rec apply_k (k : 'a cont) (v : 'a list) : 'a command =
  match k with
  | Halt -> Done v (* 计算完成，返回 Done 指令 *)
  | HandleRight (Node (node_v, _, node_r), k_rest) ->
      Compute (node_r, DoAppend (node_v, v, k_rest)) (* 返回处理右子树的指令 *)
  | HandleRight (Empty, _) -> failwith "Invalid state: HandleRight received Empty tree"
  | DoAppend (node_v, left_val, k_rest) ->
      apply_k k_rest (node_v :: (left_val @ v)) (* 尾递归调用，解开续延栈 *)

(* worker 也只返回指令 *)
let worker (t : 'a tree) (k : 'a cont) : 'a command =
  match t with
  | Empty -> apply_k k []
  | Node (_, left, _) -> Compute (left, HandleRight (t, k))

(* 蹦床是一个简单的尾递归循环 *)
let preorder_trampoline (t : 'a tree) : 'a list =
  let rec trampoline (cmd : 'a command) : 'a list =
    match cmd with
    | Done result -> result (* 收到 Done 指令，返回最终结果 *)
    | Compute (tree, k) -> trampoline (worker tree k) (* 收到 Compute 指令，执行 worker 并用新指令继续循环 *)
  in
  (* 启动蹦床 *)
  trampoline (Compute (t, Halt))

(* 版本五单循环版本,合并 apply_k 和 worker *)

(* 续延类型和之前一样 *)
type 'a cont =
  | Halt
  | HandleRight of 'a tree * 'a cont
  | DoAppend of 'a * 'a list * 'a cont

(* 新的工作项类型，用于统一的蹦床循环 *)
type 'a work_item =
  | ProcessTree of 'a tree (* 代表一个需要向下遍历的任务 *)
  | HandleValue of 'a list (* 代表一个需要向上返回的任务 *)

let preorder_inline (t : 'a tree) : 'a list =
  (* 统一的蹦床循环 *)
  let rec loop (item : 'a work_item) (k : 'a cont) : 'a list =
    match item with
    (* 情况 A: 如果工作项是 ProcessTree，执行【Worker】的逻辑 *)
    | ProcessTree current_tree ->
        (match current_tree with
        | Empty ->
            (* 遇到空树，我们得到了一个值 '[]' *)
            (* 下一步转为 HandleValue 模式，处理这个返回值 *)
            loop (HandleValue []) k
        | Node (_, left, _) ->
            (* 树不为空，继续向左深入 *)
            (* 下一个工作项仍然是 ProcessTree，并压入新续延 *)
            loop (ProcessTree left) (HandleRight (current_tree, k)))

    (* 情况 B: 如果工作项是 HandleValue，执行【apply-k】的逻辑 *)
    | HandleValue v ->
        (match k with
        | Halt -> v (* 续延栈为空，v 就是最终结果，结束循环 *)
        | HandleRight (Node (node_v, _, node_r), k_rest) ->
            (* 左子树的值是 v，现在轮到右子树 *)
            (* 下一步转为 ProcessTree 模式，处理右子树 *)
            loop (ProcessTree node_r) (DoAppend (node_v, v, k_rest))
        | HandleRight (Empty, _) -> failwith "Invalid state: HandleRight received Empty tree"
        | DoAppend (node_v, left_val, k_rest) ->
            (* 右子树的值是 v，合并结果 *)
            let new_val = node_v :: (left_val @ v) in
            (* 下一步仍然是 HandleValue 模式，处理新生成的值 *)
            loop (HandleValue new_val) k_rest)
  in
  (* 启动循环，初始工作项是处理整棵树 *)
  loop (ProcessTree t) Halt

(* 使用例子 *)
(* 创建一个示例树
      1
     / \
    2   3
   /   / \
  4   5   6
*)
let my_tree =
  Node (1,
    Node (2,
      Node (4, Empty, Empty),
      Empty
    ),
    Node (3,
      Node (5, Empty, Empty),
      Node (6, Empty, Empty)
    )
  )

(* 预期结果: [1; 2; 4; 3; 5; 6] *)

let () =
  let res1 = preorder_rec my_tree in
  let res2 = preorder_cps my_tree (fun x -> x) in
  let res3 = worker_rec my_tree Halt in
  let res4 = preorder_trampoline my_tree in
  let res5 = preorder_inline my_tree in (* 调用新版本 *)

  Printf.printf "版本 1 (朴素递归): %s\n" (String.concat "; " (List.map string_of_int res1));
  Printf.printf "版本 2 (CPS): %s\n" (String.concat "; " (List.map string_of_int res2));
  Printf.printf "版本 3 (互递归): %s\n" (String.concat "; " (List.map string_of_int res3));
  Printf.printf "版本 4 (蹦床): %s\n" (String.concat "; " (List.map string_of_int res4));
  Printf.printf "版本 5 (内联蹦床): %s\n" (String.concat "; " (List.map string_of_int res5));

  assert (res1 = res2 && res2 = res3 && res3 = res4 && res4 = res5)
