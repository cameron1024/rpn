inductive BinaryOp where
  | add
  | sub
  | mul
  | div
deriving BEq, Repr

inductive Expr where
  | int : Int -> Expr
  | binary : Expr -> BinaryOp -> Expr -> Expr
  | error : Expr
deriving BEq, Repr

instance : Inhabited Expr where
  default := .error

def Expr.eval (e : Expr) : Except String Int := do
  match e with
  | .int i => .ok i
  | .binary lhs .add rhs => .ok ((<-eval lhs) + (<-eval rhs))
  | .binary lhs .sub rhs => .ok ((<-eval lhs) - (<-eval rhs))
  | .binary lhs .mul rhs => .ok ((<-eval lhs) * (<-eval rhs))
  | .binary lhs .div rhs => .ok ((<-eval lhs) / (<-eval rhs))
  | .error => .error "encountered error node"



