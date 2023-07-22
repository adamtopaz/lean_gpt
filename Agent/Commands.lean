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

def solveTask : Command where
  name := "solve_task"
  descr := "Solve the given task.
You should only use this once you have enough information to solve the given task.
The provided solution *must* follow the schema of the given task."
  schema := .mkObj [
    ("type", "object"),
    ("properties", .mkObj [
      ("solution", .mkObj [("$ref","schema")])
    ]),
    ("required", Lean.toJson ["solution"])
  ]
  sampleUsage := .mkObj [
    ("command","solve_tasks"),
    ("param",.mkObj [
      ("solution", "Hello World!")
    ])
  ]
  exec param := do
    let .ok solution := param.getObjValAs? Lean.Json "solution" | return
    modify fun state => { 
      state with
      solution := some { solution := solution }
    } 

end Agent