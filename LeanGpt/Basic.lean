import LeanGpt.Types

open Lean

open GPT

abbrev GPTM := StateRefT (Array GPT.Message) (ReaderT GPT.Config IO)

namespace GPTM

def run (m : GPTM α) (ms : Array GPT.Message) : IO α := do
  let some apiKey ← IO.getEnv "OPENAI_API_KEY" | 
    throw <| .userError "Failed to fetch API Key"
  let e ← ReaderT.run (StateRefT'.run m ms) { apiKey := apiKey }
  return e.fst 

def getJsonResponse (req : Json) : IO Json := do
  let some apiKey ← IO.getEnv "OPENAI_API_KEY" | 
    throw <| .userError "Failed to fetch OpenAI API key"
  let child ← IO.Process.spawn {
    cmd := "curl"
    args := #[ 
      "https://api.openai.com/v1/chat/completions", 
      "-H", "Content-Type: application/json",
      "-H", "Authorization: Bearer " ++ apiKey, 
      "--data-binary", "@-"]
    stdin := .piped
    stderr := .piped
    stdout := .piped
  }
  let (stdin,child) ← child.takeStdin
  stdin.putStr <| toString req
  stdin.flush
  let stdout ← IO.asTask child.stdout.readToEnd .dedicated
  let err ← child.stderr.readToEnd
  let exitCode ← child.wait
  if exitCode != 0 then throw <| .userError err
  let out ← IO.ofExcept stdout.get
  match Json.parse out with
  | .ok json => return json
  | .error err => throw <| .userError s!"{err}\n{req}"

def getResponses (n : Nat) : GPTM (Array GPT.Message) := do
  let req : Json := 
    Json.mkObj [
      ("model", "gpt-4"),
      ("messages", toJson <| ← getThe (Array GPT.Message)),
      ("n", n)
    ]
  let jsonResponse ← getJsonResponse req
  let .ok choices := jsonResponse.getObjValAs? (Array Json) "choices" | 
    throw <| .userError s!"Failed to parse choices as array:
{jsonResponse}"
  let .ok choices := choices.mapM fun j => j.getObjValAs? GPT.Message "message" | 
    throw <| .userError s!"Failed to parse messages:
{choices}"
  return choices

def getResponse : GPTM GPT.Message := do
  let msgs ← getResponses 0 
  let some msg := msgs[0]? | 
    throw <| .userError s!"No messages were returned." 
  return msg

def streamResponse : GPTM Unit := do
  let req : Json := Json.mkObj [
      ("model", "gpt-4"),
      ("messages", toJson <| ← get),
      ("stream", true)
  ]
  let child ← IO.Process.spawn {
    cmd := "curl"
    args := #[ 
      "https://api.openai.com/v1/chat/completions", "-N", 
      "-H", "Content-Type: application/json",
      "-H", "Authorization: Bearer " ++ (← read).apiKey, 
      "--data-binary", "@-"]
    stdin := .piped
    stderr := .piped
    stdout := .piped
  }
  let (stdin,child) ← child.takeStdin
  stdin.putStr <| toString req
  stdin.flush
  let stdout ← IO.getStdout
  while true do
    let res ← child.stdout.getLine
    if res == "\n" then continue
    if res == "" then break
    let some res := (res.splitOn "data: ")[1]? | continue
    if res == "[DONE]\n" then break
    let .ok json := Json.parse res | continue
    let .ok choices := json.getObjValAs? (Array Json) "choices" | continue
    let some choice := choices[0]? | continue
    let .ok delta := choice.getObjValAs? Json "delta" | continue
    let .ok content := delta.getObjValAs? String "content" | continue
    stdout.putStr content
    stdout.flush
  let err ← child.stderr.readToEnd
  let exitCode ← child.wait
  if exitCode != 0 then throw <| .userError err

end GPTM