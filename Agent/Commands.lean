import Agent.Types

namespace Agent

def greet : Command where
  name := "greet"
  descr := "Send a greeting"
  schema := .mkObj [
    ("type", "string")
  ]
  sampleUsage := .mkObj [
    ("command","greet"),
    ("param","Hello World!")
  ]
  exec param := do
    let .str greeting := param | return
    modify fun state => { 
      state with 
      workingMems := 
        state.workingMems.push { 
          name := "greet" 
          descr := "Output of greet command"
          content := greeting
        }
    }

def removeTasks : Command where
  name := "removeTask"
  descr := "Remove all tasks with a given name from the task list.
You should use this to remove tasks which have been completed."
  schema := .mkObj [
    ("type", "string")
  ]
  sampleUsage := .mkObj [
    ("command","removeTask"),
    ("param", "task_name")
  ]
  exec param := do
    let .str task_name := param | return
    modify fun state => {
      state with tasks := state.tasks.filter fun t => t.name != task_name
    }

def solveTasks : Command where
  name := "solve_tasks"
  descr := "Solve all tasks with a given name rom the task list.
You should only use this once you have enough information to solve the given tasks."
  schema := .mkObj [
    ("type", "object"),
    ("properties", .mkObj [
      ("task_name", .mkObj [("type","string")]),
      ("solution", .mkObj [("type","string")])
    ]),
    ("required", Lean.toJson ["task_name","solution"])
  ]
  sampleUsage := .mkObj [
    ("command","solve_tasks"),
    ("param",.mkObj [
      ("task_name", "send_greeting"),
      ("solution", "Hello World!")
    ])
  ]
  exec param := do
    let .ok taskName := param.getObjValAs? String "task_name" | return
    let .ok solution := param.getObjValAs? Lean.Json "solution" | return
    modify fun state => { 
      state with
      tasks := Id.run do 
        let mut out := #[]
        for i in state.tasks do 
          out := out.push <| if i.name == taskName 
            then { i with solution := solution } 
            else i
        return out
    } 

end Agent