import Init.Data.List.Sublist

import Rpn.Util

inductive Token
  | error : Token
  | whitespace : Token
  | add : Token
  | sub : Token
  | mul : Token
  | div : Token
  | int : Int -> Token
deriving Repr, BEq

instance : ToString Token where
  toString 
  | .error => "<<error>>"
  | .whitespace => " "
  | .add => "+"
  | .sub => "-"
  | .mul => "*"
  | .div => "/"
  | .int i => toString i


private def digitsToInt (digits : List { c : Char // c.isDigit }) : Int :=
  let powersOfTen : List Int := (List.range digits.length).map (( 10 : Int ).pow ·)
  let digits : List Int := (digits.reverse.map (fun c => c.val.val - '0'.val)).map (fun i => i.toNat)
  ((digits.zip powersOfTen).map (fun ⟨ l, r ⟩ => l * r)).sum


/- theorem List.dropWhile_lt { α : Type u } (as : List α) (p : α -> Bool) : (as.dropWhile p).length <= as.length := by -/
/-   refine Sublist.length_le ?_ -/
/-   exact dropWhile_sublist p -/

        
private def lexImpl (input : List Char) : List Token :=
  match input with
  | [] => []
  | c :: cs => 
    match c with 
    | '+' => .add :: (lexImpl cs)
    | '-' => .sub :: (lexImpl cs)
    | '*' => .mul :: (lexImpl cs)
    | '/' => .div :: (lexImpl cs)
    | other => 
      if is_digit : other.isDigit 
      then
        let head := cs.headMatching (·.isDigit)
        let tail := cs.drop head.length

        let digits := ⟨ other, is_digit ⟩ :: head
        let int := digitsToInt digits
        .int int :: (lexImpl tail) 
      else if _is_whitespace : other.isWhitespace 
      then
        let head := cs.headMatching (·.isWhitespace)
        let tail := cs.drop head.length
        .whitespace :: (lexImpl tail)
      else 
        .error :: (lexImpl cs)


  termination_by input.length 

def lex (input : String) : List Token := input |> String.toList |> lexImpl




