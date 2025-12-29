inductive BinaryOp where
  | add
  | sub
  | mul
  | div
deriving BEq, Repr

instance : ToString BinaryOp where
  toString 
    | .add => "+"
    | .sub => "-"
    | .mul => "*"
    | .div => "/"

inductive Expr where
  | int : Int -> Expr
  | binary : Expr -> BinaryOp -> Expr -> Expr
  | error : Expr
deriving BEq, Repr

instance : Inhabited Expr where
  default := .error

private def String.replicate (length : Nat) (char : Char) : String := 
  (List.replicate length char).asString

private def Expr.toStringPretty' (e : Expr) (depth : Nat) : String :=
  let padding := String.replicate (depth * 2) ' '
  match e with
  | .error => s!"{padding}<error>"
  | .int i => s!"{padding}Int: {i}"
  | .binary lhs op rhs => 
      s!"{padding}Op :{op}\n{padding}{lhs.toStringPretty' (depth + 1)}\n{padding}{rhs.toStringPretty' (depth + 1)}"


def Expr.toStringPretty (e : Expr) : String := e.toStringPretty' 0

def Expr.eval? (e : Expr) : Option Int := do
  match e with
  | .int i => i
  | .error => .none
  | .binary lhs .add rhs => (<-eval? lhs) + (<-eval? rhs)
  | .binary lhs .sub rhs => (<-eval? lhs) - (<-eval? rhs)
  | .binary lhs .mul rhs => (<-eval? lhs) * (<-eval? rhs)
  | .binary lhs .div rhs => (<-eval? lhs) / (<-eval? rhs)

def Expr.isValid : Expr -> Bool 
  | .error              => false
  | .int _              => true
  | .binary lhs _op rhs => lhs.isValid && rhs.isValid

def Expr.children : Expr -> List Expr 
  | .error            => []
  | .int _            => []
  | .binary lhs _ rhs => [lhs, rhs] ++ lhs.children ++ rhs.children

def Expr.isError : Expr -> Bool
  | .error => true
  | _      => false


theorem Expr.isValid_deep 
  (lhs rhs : Expr) 
  (op : BinaryOp) 
  : (Expr.binary lhs op rhs).isValid -> (lhs.isValid ∧ rhs.isValid) := by 
    intro is_valid
    exact Bool.and_eq_true_iff.mp is_valid

def Expr.eval (e : Expr) (is_valid : e.isValid) : Int := 
  match e with
  | .int i => i
  | .binary lhs op rhs => 
    let ⟨ lhs_valid, rhs_valid ⟩ := Expr.isValid_deep lhs rhs op is_valid

    let lhs := eval lhs lhs_valid
    let rhs := eval rhs rhs_valid

    match op with
    | .add => lhs + rhs
    | .sub => lhs - rhs
    | .mul => lhs * rhs
    | .div => lhs / rhs

    

