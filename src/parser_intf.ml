module Impl = struct
  type parse_error =
    | UnknownOperation of int * string
    | MissingHyphen of int
    | MissingEquals of int
    | BadNumber of int * string
    | BadKeyName of int * string
    | BadKeypressType of int * string
    | BadUnicode of int * string
  [@@deriving show { with_path = false }]
end

module type Intf = sig
  include module type of Impl
end
