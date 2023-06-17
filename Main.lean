import LeanGpt

def main (args : List String) : IO Unit := do
  let input := args.intersperse " " |>.foldl (· ++ ·) ""
  GPTM.streamResponse.run #[
    {role := .system, content := 
"You are a helpful assistant.
Respond to the user's query."},
    {role := .user, content := input}
    ]
  --IO.println out.content