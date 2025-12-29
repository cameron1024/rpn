import Init.Data.List.Sublist

def List.headMatching { α : Type u } (as : List α) (p : α -> Bool) : List { a : α // p a } :=
  match as with
  | [] => []
  | a :: as =>
    if proof : p a 
    then ⟨ a, proof ⟩ :: (as.headMatching p)
    else []

structure Span where
  source         : String
  start          : source.ValidPos
  end'           : source.ValidPos
  start_lt_end   : start < end'
deriving BEq

structure Spanned (α : Type u) where
  val  : α
  span : Span
deriving BEq

instance : Repr Span where
  reprPrec span _prec := .text s!"{span.start.offset.byteIdx}..{span.end'.offset.byteIdx}"

def Span.new (source : String) (startByte : Nat) (endByte : Nat) : Option Span := do
  let start <- source.pos? ⟨ startByte ⟩ 
  let end'  <- source.pos? ⟨ endByte ⟩ 

  if start_lt_end : start < end' 
  then .some ⟨ source, start, end', start_lt_end ⟩ 
  else .none


private theorem pos_lt_imp_le (s : String) (a b : s.ValidPos) : a < b -> a <= b := by
  intro a_lt_b
  refine String.ValidPos.le_iff.mpr ?_
  have a_lt_b_offset : a.offset < b.offset := a_lt_b
  exact String.Pos.Raw.le_of_lt a_lt_b


def Span.text (span : Span) : String.Slice :=
  ⟨ 
    span.source,
    span.start,
    span.end',
    (pos_lt_imp_le span.source span.start span.end') span.start_lt_end
  ⟩ 

def Span.lengthBytes (span : Span) : Nat := span.text.utf8ByteSize
def Span.chars (span : Span) : List Char := span.text.chars.toList
