import Lean

open Lean

namespace GPT

structure Config where
  apiKey : String
deriving ToJson, FromJson

inductive Role where | system | user | assistant
deriving ToJson, FromJson

structure Message where
  role : Role
  content : String 
deriving ToJson, FromJson

end GPT