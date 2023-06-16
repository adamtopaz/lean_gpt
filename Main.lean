import LeanGpt

def main (args : List String) : IO Unit := do
  let input := args.intersperse " " |>.foldl (· ++ ·) ""
  let out ← GPTM.getResponse.run #[
    {role := .system, content := 
"You are a helpful assistant.
Respond to the user's query."},
    {role := .user, content := input}
    ]
  IO.println out.content