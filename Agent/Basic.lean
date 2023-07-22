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
  let task ← getThe Task
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
  commands := #[checkTemperature, solveTask]

partial def solveTask (trace : Bool := false) : AgentM Solution := do
  --if (← getThe (Array Task)).size == 0 then return
  if trace then
    IO.println "PROMPT"
    IO.println "======"
    IO.println "======"
    IO.println ""
    IO.println (← systemPrompt)
    IO.println "HISTORY"
    IO.println "======"
    IO.println "======"
    IO.println ""
    IO.println <| toJson (← getThe (Array GPT.Message))

  if let some sol := (← getThe (Option Solution)) then 
    return sol

  let res ← getMsg 
  if trace then
    IO.println "RESPONSE"
    IO.println "========"
    IO.println "========"
    IO.println ""
    IO.println (toJson res)

  let .ok res := Json.parse res.content | solveTask 
  let .ok cmd := res.getObjValAs? String "command" | solveTask
  let .ok param := res.getObjValAs? Json "param" | solveTask
  let cmds ← readThe (Array Command) 
  let some cmd := cmds.find? fun c => c.name == cmd | solveTask
  cmd.exec param
  solveTask

end AgentM

open AgentM

def main : IO Unit := do
  let e : AgentM String := do
    let sol ← solveTask
    return s!"{toJson sol}"
  let out ← e.run mainCfg
    { task := 
        { 
          name := "find_temp" 
          spec := "Find the temperature in a random city in Europe"
          schema := .mkObj [
            ("type","object"),
            ("properties",.mkObj [
              ("city", .mkObj [("type","string")]),
              ("temp", .mkObj [("type","int")])
            ])
            ]
        }
    }
  IO.println <| out
