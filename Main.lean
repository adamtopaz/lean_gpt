import LeanGpt

partial def loop (stdin : IO.FS.Stream) : GPTM Unit := do
  IO.print "User: "
  let prompt ← stdin.getLine
  if prompt.trim == "QUIT" then return
  modify fun hist => hist.push { content := prompt }
  IO.print "Assistant: "
  GPTM.streamResponse
  IO.println ""
  loop stdin

def main : IO Unit := do
  let stdin ← IO.getStdin
  (loop stdin).run #[{ role := .system, content := "You are a helpful assistant."}]