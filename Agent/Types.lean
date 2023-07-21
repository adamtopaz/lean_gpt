import Lean
import LeanGpt

open Lean

namespace Agent

structure WorkingMem where

structure Observation where

structure State where
  workingMems : Array WorkingMem := #[]
  obs : Array Observation := #[]
  history : Array GPT.Message := #[]

structure Command where
  name : String
  descr : String
  schema : Json
  exec : Json → StateRefT State IO Unit

structure Config where
  systemBase : String := ""
  commands : Array Command := #[]
  
end Agent

open Agent

abbrev AgentM := ReaderT Config (StateRefT State IO)

namespace AgentM

instance : MonadStateOf (Array WorkingMem) AgentM where
  get := do
    let state ← getThe State
    return state.workingMems
  set e := do
    let state ← getThe State
    set { state with workingMems := e }
  modifyGet f := do 
    let state ← getThe State
    let (out, mems) := f state.workingMems
    set { state with workingMems := mems }
    return out

instance : MonadStateOf (Array Observation) AgentM where
  get := do
    let state ← getThe State
    return state.obs
  set e := do
    let state ← getThe State
    set { state with obs := e }
  modifyGet f := do
    let state ← getThe State
    let (out, obs) := f state.obs
    set { state with obs := obs }
    return out

instance : MonadStateOf (Array GPT.Message) AgentM where
  get := do
    let state ← getThe State
    return state.history
  set e := do
    let state ← getThe State
    set { state with history := e }
  modifyGet f := do
    let state ← getThe State
    let (out, hist) := f state.history
    set { state with history := hist }
    return out

def systemPrompt : AgentM String := return ""

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

end AgentM