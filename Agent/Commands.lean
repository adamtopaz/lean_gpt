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

end Agent