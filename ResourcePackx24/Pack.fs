module Pack

open System
open System.IO
open System.Threading
open FSharp.Json
open Config

type NodeType = File | Directory
type PackLock = (string*(NodeType*DateTime))[]
type PackLockMap = Map<string, NodeType*DateTime>

let get_cutdown_filename (file:string) = file.Replace(config.source, "")
let get_cutdown_filename_from_desintation (file:string) = file.Replace(config.desintation, "")
let get_input_filename (file:string) = Path.Combine(config.source, get_cutdown_filename file)
let get_output_filename (file:string) = Path.Combine(config.desintation, get_cutdown_filename file)

let rec discover_files (directory:string):PackLock =
  let files =
    Directory.GetFiles directory
    |> Array.map(fun file ->
      let file_info = FileInfo(file)
      (get_cutdown_filename file, (NodeType.File, file_info.LastWriteTimeUtc))
    )
  let inner_nodes =
    Directory.GetDirectories directory
    |> Array.collect(fun dir ->
      discover_files dir
    )
  let dir_info = DirectoryInfo directory
  files
  |> Array.append (
    [|(
      get_cutdown_filename directory,
      (NodeType.Directory, dir_info.LastWriteTimeUtc)
    )|]
  )
  |> Array.append inner_nodes

let fetch_new_pack_lock directory:PackLockMap =
  discover_files directory
  |> Array.filter(fun (k, _) -> k <> "pack.lock" && k <> "")
  |> Map.ofArray

let mutable pack_lock:PackLockMap = Map.empty

let save_pack_lock directory =
  File.WriteAllText(Path.Combine(directory, "pack.lock"), Json.serialize pack_lock)

let fetch_pack_lock directory:PackLockMap =
  if pack_lock.Count = 0 then
    let uri = (Path.Combine(directory, "pack.lock"))
    if File.Exists uri then
      pack_lock <-
        File.ReadAllText uri
        |> Json.deserialize
      pack_lock
    else Map.empty
  else pack_lock

let modify_pack_lock directory file file_type date =
  if pack_lock.Count = 0 then fetch_new_pack_lock directory |> ignore
  if pack_lock.ContainsKey file then
    pack_lock <-
      pack_lock
      |> Map.map(fun k (t, d) ->
        if k = file then (file_type, date)
        else (t, d)
      )
  else
    pack_lock <-
      pack_lock.Add(file, (file_type, date))

let remove_from_pack_lock directory file =
  if pack_lock.Count = 0 then fetch_new_pack_lock directory |> ignore
  if pack_lock.ContainsKey file then
    pack_lock <-
      pack_lock
      |> Map.filter(fun k _ -> k <> file)