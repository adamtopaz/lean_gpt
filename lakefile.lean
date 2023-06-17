import Lake
open Lake DSL

package «lean_gpt» {
  -- add package configuration options here
}

lean_lib «LeanGpt» {
  -- add library configuration options here
}

@[default_target]
lean_exe «askgpt» {
  root := `Main
}
