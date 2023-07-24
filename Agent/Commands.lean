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

def lookupInfo : Command where
  name := "lookup_info"
  descr := "Look up information using GPT4.
Keep in mind that GPT4 is not really made for such requests, so make sure to only look for general information that you know would have been part of GPT4's training data.

The sample usage explains how to look up information about cats."
  schema := .mkObj [
    ("type","string")
  ]
  sampleUsage := .mkObj [
    ("command","lookup_info"),
    ("param","Cats")
  ]
  exec jsn := do
    let .str inpt := jsn | return
    let info : GPT.Message ← GPTM.getResponse.run 
      #[
        { role := .system, content := "You are an advanced search engine which gives information about the user's query gathered from multiple source.
If a user's message says \"Cats\", then give information about cats.
If a user's message says \"the Riemann Hypothesis\", then give information aobut the Riemann hypothesis." },
        { role := .user, content := inpt }
      ]
    let mem : WorkingMem := {
      name := "info"
      descr := s!"Information about {inpt}"
      content := info.content
    }
    modify fun state => { state with workingMems := state.workingMems.push mem }

def checkTemperature : Command where
  name := "check_temp"
  descr := "Check the temperature in a given location. Make sure to remove any spaces from the name of the city for which you want the remperature." 
  schema := .mkObj [
    ("type", "string")
  ]
  sampleUsage := .mkObj [
    ("command","check_temp"),
    ("param", "London")
  ]
  exec param := do
    let .str loc := param | return 
    let out ← IO.Process.output {
      cmd := "curl"
      args := #[s!"wttr.in/{loc}?format=%t"]
    }
    modify fun state => {
      state with 
      workingMems := state.workingMems.push 
        { 
          name := "temp" 
          descr := s!"The temperature at {loc}"
          content := out.stdout
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
    ("command","solve_task"),
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