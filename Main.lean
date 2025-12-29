import Rpn.Parser

def processLine (line : String) : Option Int :=
  (parse line).eval?

partial def repl (stream : IO.FS.Stream) : IO Unit := do
  let line <- stream.getLine

  match processLine line with
  | .some int => IO.println int
  | .none => IO.eprintln "error"

  repl stream


def main : IO Unit := do
  let stdin <- IO.getStdin
  repl stdin

