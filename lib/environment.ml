module StringMap = Map.Make(String)

type environment = {
  mutable store : Object.obj StringMap.t;
}

let get env name =
  match StringMap.find_opt name env.store with
  | Some obj -> Ok obj
  | None -> Error ("No variable " ^ name ^ " found")

let set env name obj =
  env.store <- env.store |> StringMap.add name obj

let init = {
  store = StringMap.empty;
}