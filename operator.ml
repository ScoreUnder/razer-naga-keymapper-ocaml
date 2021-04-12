type operator =
  | Chmap
  | Key
  | Run
  | Run2
  | Click
  | Workspace
  | WorkspaceR
  | Position
  | Delay
  | Media
  | Toggle
[@@deriving show { with_path = false }]

let of_string err = function
  | "chmap" -> Ok Chmap
  | "key" -> Ok Key
  | "run" -> Ok Run
  | "run2" -> Ok Run2
  | "click" -> Ok Click
  | "workspace_r" -> Ok WorkspaceR
  | "workspace" -> Ok Workspace
  | "position" -> Ok Position (* + replace commas with spaces in line *)
  | "delay" -> Ok Delay
  | "media" -> Ok Media
  | "toggle" -> Ok Toggle
  | bad -> Error (err bad)
