import Init.Data.List.Sublist

def List.headMatching { α : Type u } (as : List α) (p : α -> Bool) : List { a : α // p a } :=
  match as with
  | [] => []
  | a :: as =>
    if proof : p a 
    then ⟨ a, proof ⟩ :: (as.headMatching p)
    else []
        

