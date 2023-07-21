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

def taskList : AgentM String := do
  let tasks ← getThe (Array Task)
  return formatList tasks

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
1. Tasks: The list of tasks to complete.
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

Tasks:
======

{← taskList}

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

def get : AgentM GPT.Message := do
  let history ← getThe (Array GPT.Message)
  let history := history.filter fun a => a.role != .system
  let msgs : Array GPT.Message := #[{ role := .system, content := ← systemPrompt }] ++ history
  let msg ← GPTM.getResponse.run msgs
  modifyGetThe (Array GPT.Message) fun hist => (msg, hist.push msg)

def send (msg : GPT.Message) : AgentM GPT.Message := do
  modifyThe (Array GPT.Message) fun hist => hist.push msg
  get

def main : Config where
  systemBase := "You are an autonomous agent whose goal is to solve the given tasks."
  commands := #[greet]

#eval show IO Unit from do
  IO.println <| ← systemPrompt.run main
    { tasks := 
      #[
        { 
          name := "say_hello" 
          descr := "A basic task."
          content := "Say hello."}
      ] 
    }

end AgentM