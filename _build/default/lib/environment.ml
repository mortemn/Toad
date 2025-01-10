module StringMap = Map.Make(String)

type 'a environment = {
  mutable store : 'a StringMap.t [@opaque];
  outer : 'a environment option [@opaque];
}
[@@deriving show]

let rec get env name =
  match StringMap.find_opt name env.store with
  | Some obj -> Ok obj
  | None -> (
    match env.outer with
    | Some outer -> get outer name
    | None -> Error ("Identifier not found: " ^ name)
  )

let set env name obj =
  env.store <- env.store |> StringMap.add name obj

let new_enclosed env =
  {
    store = StringMap.empty;
    outer = Some env;
  }

let init () = {
  store = StringMap.empty;
  outer = None;
}