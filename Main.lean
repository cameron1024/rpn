import Rpn.Parser

def processLine (line : String) : Except String Int :=
  (parse line).eval

partial def repl (stream : IO.FS.Stream) : IO Unit := do
  let line <- stream.getLine

  match processLine line with
  | .ok int => IO.println int
  | .error msg => IO.eprintln msg

  repl stream


def main : IO Unit := do
  let stdin <- IO.getStdin
  repl stdin

