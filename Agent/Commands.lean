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

end Agent