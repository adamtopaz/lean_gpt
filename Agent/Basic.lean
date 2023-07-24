import Agent.Types
import Agent.Commands

open Agent
open Lean (Json toJson ToJson)

namespace AgentM

def newline : String := 
"

"

def formatList {α : Type} [ToJson α] (A : Array α) : String := 
  A.map (toString ∘ toJson) |>.toList |>.intersperse newline |>.foldl (· ++ ·) ""

def task : AgentM String := do
  let task ← readThe Task
  return s!"{toJson task}"

def commandList : AgentM String := do
  let obs ← readThe (Array Command)
  return formatList obs

def workingMemList : AgentM String := do
  let obs ← getThe (Array WorkingMem)
  return formatList obs

def observationList : AgentM String := do
  let obs ← getThe (Array Observation)
  return formatList obs

def responseSchema : Json := .mkObj [
  ("$schema",  "http://json-schema.org/draft-07/schema#"),
  ("type", "object"),
  ("properties", .mkObj [
    ("command", .mkObj [("type", "string")]),
    ("param", .mkObj [("$ref", "schema")])
  ]),
  ("required", toJson ["command", "param"])
]

def systemPrompt : AgentM String := do
  let base ← readThe String 
  return s!"{base}

This system message contains the following information, each in its own section:
1. Task: The current task to complete.
2. Commands: The list of available commands.
3. Working Memory: Your working memory.
4. Observations: A collection of observations.

Every response must use one of the given commands.
Responses must be valid json satisfying the following schema:

{responseSchema}

The schema for the param field in your responses must follow the schema descried in the command you want to use.
For example, if a command called \"greet\" has the following description:

{toJson greet}

then to use this command to send a greeting saying \"Hello world!\", you must respond with the following:

{greet.sampleUsage}

Each command description has a field called \"sampleUsage\" providing an example illustrating how to use the given command.

Task:
======

{← task}

Commands:
=========

{← commandList}

Working Memory:
===============

{← workingMemList}

Observations:
=============

{← observationList}
"

def runState (m : AgentM α) (cfg : Config := {}) (state : State := {}) : IO (α × State) := 
  StateRefT'.run (ReaderT.run m cfg) state

def run (m : AgentM α) (cfg : Config := {}) (state : State := {}) : IO α := 
  m.runState cfg state <&> Prod.fst

def getMsg : AgentM GPT.Message := do
  let history ← getThe (Array GPT.Message)
  let history := history.filter fun a => a.role != .system
  let msgs : Array GPT.Message := #[{ role := .system, content := ← systemPrompt }] ++ history
  let msg ← GPTM.getResponse.run msgs
  modifyGetThe (Array GPT.Message) fun hist => (msg, hist.push msg)

def send (msg : GPT.Message) : AgentM GPT.Message := do
  modifyThe (Array GPT.Message) fun hist => hist.push msg
  getMsg

def mainCfg : Config where
  systemBase := "You are an autonomous agent whose goal is to solve the given tasks."
  commands := #[checkTemperature, solveTask, lookupInfo]

partial def solveTask (handle : IO.FS.Handle) 
    (trace : Bool := true) (interactive := true) : AgentM Solution := do
  --if (← getThe (Array Task)).size == 0 then return
  let logWrite : String → IO Unit := fun s => do
    IO.println s 
    handle.putStrLn s
  if trace then
    logWrite "PROMPT:"
    logWrite <| ← systemPrompt
    logWrite "HISTORY:"
    logWrite <| toString <| toJson <| ← getThe (Array GPT.Message)
    logWrite "SOLUTION:"
    logWrite <| toString <| toJson <| ← getThe (Option Solution)

  if let some sol := (← getThe (Option Solution)) then 
    return sol

  let res ← getMsg 

  if trace then
    logWrite "RESPONSE:"
    logWrite <| toString <| toJson res

  let .ok res := Json.parse res.content | solveTask handle 
  let .ok cmd := res.getObjValAs? String "command" | solveTask handle
  let .ok param := res.getObjValAs? Json "param" | solveTask handle
  let cmds ← readThe (Array Command) 
  let some cmd := cmds.find? fun c => c.name == cmd | solveTask handle
  if interactive then
    let stdin ← IO.getStdin 
    IO.print "Continue? (Y/n): "
    let line ← stdin.getLine
    if line.trim == "Y" || line.trim = "" then 
      cmd.exec param
  else
    cmd.exec param
  solveTask handle

end AgentM

open AgentM

def fname : IO System.FilePath := do
  let dt ← IO.Process.output {
    cmd := "date"
    args := #["+%Y-%m-%d::%H:%M:%S"]
  } <&> (·.stdout)
  return dt.trim ++ ".log"

def main : IO Unit := do IO.FS.withFile (← fname) .write fun handle => do
  let e : AgentM String := do
    let sol ← solveTask handle
    return s!"{toJson sol}"
  let out ← e.run 
    { mainCfg with
      task := 
        { 
          name := "find_temp" 
          spec := "If X, Y and Z are the top three cities in the world in terms of population, and foo(CityName) is the current temperature at CityName, then please give me the sum foo(X) + foo(Y) + foo(Z) + B, where B will be defined in the next line.
Lookup the number of isomorphism classes of abelian groups of order 12, and set B to be that number.
Your final answer should be a single number."
          schema := .mkObj [
            ("type","int")
          ]
        }
    }
  IO.println <| out
