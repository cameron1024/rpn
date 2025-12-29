import Rpn.Lexer
import Rpn.Expr

private def parseExpr (input : List Token) : { output : (List Token) × Expr // output.fst.length < input.length + 1 } := 
  match input with
  | [] => ⟨ ([], .error), Nat.lt_add_one ([], Expr.error).fst.length ⟩ 
  | .error      :: _ => ⟨ ([], .error), Nat.lt_of_sub_eq_succ rfl ⟩ 
  | .whitespace :: ts => 
    let out := parseExpr ts
    ⟨ out.val , by 
      have h1 := out.property
      exact Nat.lt_add_right 1 h1
    ⟩ 
  | .int i      :: ts => 
    ⟨ (ts, .int i), by grind only [= List.length_cons] ⟩ 
  | .add        :: ts => 
    let ⟨ (ts1, rhs), h1 ⟩ := parseExpr ts
    let ⟨ (ts2, lhs), h2 ⟩  := parseExpr ts1
    let expr  := .binary lhs .add rhs
    ⟨ (ts2, expr), by grind ⟩ 
  | .sub        :: ts => 
    let ⟨ (ts1, rhs), h1 ⟩ := parseExpr ts
    let ⟨ (ts2, lhs), h2 ⟩  := parseExpr ts1
    let expr  := .binary lhs .sub rhs
    ⟨ (ts2, expr), by grind ⟩ 
  | .mul        :: ts => 
    let ⟨ (ts1, rhs), h1 ⟩ := parseExpr ts
    let ⟨ (ts2, lhs), h2 ⟩  := parseExpr ts1
    let expr  := .binary lhs .mul rhs
    ⟨ (ts2, expr), by grind ⟩ 
  | .div        :: ts => 
    let ⟨ (ts1, rhs), h1 ⟩ := parseExpr ts
    let ⟨ (ts2, lhs), h2 ⟩  := parseExpr ts1
    let expr  := .binary lhs .div rhs
    ⟨ (ts2, expr), by grind ⟩ 

  termination_by input.length


def parse (input : String) : Expr := 
  let tokens := lex input
  let result :=  parseExpr tokens.reverse
  let (remainingTokens, expr) := result.val
  if remainingTokens == [] then expr else .error
  
    
  
  



  

  

