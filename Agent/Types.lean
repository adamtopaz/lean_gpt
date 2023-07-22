import Lean
import LeanGpt

open Lean (Json ToJson FromJson)

namespace Agent

structure WorkingMem where
  name : String
  descr : String
  content : String
deriving ToJson, FromJson

structure Observation where
  name : String
  descr : String
  content : String
deriving ToJson, FromJson

structure Task where
  name : String
  descr : String
  content : String
  solution : Option Json := none
  solutionSchema : Json
deriving ToJson, FromJson

structure State where
  tasks : Array Task := #[]
  workingMems : Array WorkingMem := #[]
  obs : Array Observation := #[]
  history : Array GPT.Message := #[]

structure Command where
  name : String
  descr : String
  schema : Json
  sampleUsage : Json
  exec : Json → StateRefT State IO Unit

instance : ToJson Command where
  toJson cmd := .mkObj [
    ("name", cmd.name),
    ("desc", cmd.descr),
    ("schema", cmd.schema),
    ("sampleUsage", cmd.sampleUsage)
  ]

structure Config where
  systemBase : String := ""
  commands : Array Command := #[]

end Agent

open Agent

abbrev AgentM := ReaderT Config (StateRefT State IO)

namespace AgentM

instance : MonadStateOf (Array Task) AgentM where
  get := do
    let state ← getThe State
    return state.tasks
  set e := do
    let state ← getThe State
    set { state with tasks := e }
  modifyGet f := do 
    let state ← getThe State
    let (out, tasks) := f state.tasks
    set { state with tasks := tasks }
    return out

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

instance : MonadReaderOf (Array Command) AgentM where
  read := do
    let cfg ← readThe Config
    return cfg.commands

instance : MonadReaderOf String AgentM where
  read := do
    let cfg ← readThe Config
    return cfg.systemBase

end AgentM