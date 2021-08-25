open System
open System.Collections.Generic;
open System.Drawing
open System.IO
open System.Threading
open FSharp.Json
open BlackFox.ColoredPrintf

open Config
open Pack

let exitEvent = new ManualResetEvent false

let SIZE_FILTER = 24
let SIZE_FILTER_FLOAT = float SIZE_FILTER

let rec bitmap_from_file path = //Avoids keeping file open
  try
    new MemoryStream(File.ReadAllBytes path)
    |> Image.FromStream
  with
  | _ -> bitmap_from_file path

let file_needs_resizing (file:string) =
  if Path.GetExtension file = ".png" then
    let bmp = bitmap_from_file file
    bmp.Width = SIZE_FILTER && bmp.Height = SIZE_FILTER
  else false

let resize_bitmap file:Bitmap =
  let new_size = config.output_size/SIZE_FILTER_FLOAT |> floor |> (*) SIZE_FILTER_FLOAT |> int
  let output_size = int config.output_size
  let offset = (output_size - new_size)/2
  let res = new Bitmap(output_size,output_size)
  let g = Graphics.FromImage res
  g.InterpolationMode <- Drawing2D.InterpolationMode.NearestNeighbor
  g.PixelOffsetMode <- Drawing2D.PixelOffsetMode.HighQuality
  let rect = new Rectangle(offset, offset, new_size, new_size)
  g.DrawImage(bitmap_from_file file, rect)
  res

let rec duplicate_file uri =
  try 
    let input = get_input_filename uri
    let output = get_output_filename uri

    if Directory.Exists input then
      if Directory.Exists output |> not then
        colorprintfn "$green[Creating directory] %s" uri
        Directory.CreateDirectory output |> ignore
      modify_pack_lock config.source uri NodeType.Directory (DirectoryInfo(input).LastWriteTimeUtc)
    elif file_needs_resizing input then
      colorprintfn "$cyan[Resizing] %s" uri
      let bmp = resize_bitmap input
      bmp.Save output
      modify_pack_lock config.source uri NodeType.File (FileInfo(input).LastWriteTimeUtc)
    elif File.Exists output |> not then
      colorprintfn "$yellow[Duplicating file] %s" uri
      File.Copy(input, output)
      modify_pack_lock config.source uri NodeType.File (FileInfo(input).LastWriteTimeUtc)
    elif
      File.Exists output
      && pack_lock.ContainsKey uri
        && FileInfo(input).LastWriteTimeUtc <> snd pack_lock.[uri] then
        colorprintfn "$darkyellow[Replacing file] %s" uri
        File.Copy(input, output, true)
        modify_pack_lock config.source uri NodeType.File (FileInfo(input).LastWriteTimeUtc)
    else
      modify_pack_lock config.source uri NodeType.File (FileInfo(input).LastWriteTimeUtc)
  with
  | _ -> duplicate_file uri

let duplicate_directory uri =
  fetch_new_pack_lock uri
  |> Map.toList
  |> List.iter(fun (k, (t,_)) ->
    if File.Exists k then
      duplicate_file k
    elif Directory.Exists k then
      get_cutdown_filename_from_desintation k
      |> colorprintfn "$green[Creating directory]:%s"
      get_output_filename k
      |> Directory.CreateDirectory
      |> ignore
  )

let delete_file uri =
  let output = get_output_filename uri
  if Directory.Exists output then
    Directory.Delete output
  elif File.Exists output then
    File.Delete output

let delete_directory uri =
  fetch_new_pack_lock uri
  |> Map.toList
  |> List.rev
  |> List.iter(fun (k, (t,_)) ->
    if File.Exists k then
      get_cutdown_filename_from_desintation k
      |> colorprintfn "$red[Deleting file]:%s"
      File.Delete k
    elif Directory.Exists k then
      get_cutdown_filename_from_desintation k
      |> colorprintfn "$red[Deleting directory]:%s"
      Directory.Delete k
  )

let a_in_b (a:PackLockMap) (b:PackLockMap):PackLockMap =
  b |> Map.filter(fun k _ ->
    a.ContainsKey k
  )
let a_not_in_b (a:PackLockMap) (b:PackLockMap):PackLockMap =
  b |> Map.filter(fun k _ ->
    a.ContainsKey k |> not
  )

let init () =
  let pack_lock = fetch_pack_lock config.source
  let new_pack_lock = fetch_new_pack_lock config.source

  let additions = a_not_in_b pack_lock new_pack_lock
  let removals = a_not_in_b new_pack_lock pack_lock
  let changes =
    a_in_b new_pack_lock pack_lock
    |> Map.filter(fun k (_,v) -> 
      v <> snd pack_lock.[k]
      || v <> FileInfo(get_input_filename k).LastWriteTimeUtc
    )

  //printfn "Additions: %i\nRemovals:  %i\nChanges:   %i" additions.Count removals.Count changes.Count

  additions
  |> Map.iter(fun k _ ->
    duplicate_file k
  )
  removals
  |> Map.iter(fun k _ ->
    delete_file k
  )
  changes
  |> Map.iter(fun k _ ->
    duplicate_file k
  )

  save_pack_lock config.source
  printfn "Initialisation finished"
  
let watch_directory_for_changes pack_directory changed created deleted renamed =
  let watcher = new FileSystemWatcher(pack_directory)
  
  watcher.NotifyFilter <-
    NotifyFilters.LastWrite
    ||| NotifyFilters.FileName
    ||| NotifyFilters.DirectoryName
  
  watcher.Changed.AddHandler (new FileSystemEventHandler(changed))
  watcher.Created.AddHandler (new FileSystemEventHandler(created))
  watcher.Deleted.AddHandler (new FileSystemEventHandler(deleted))
  watcher.Renamed.AddHandler (new RenamedEventHandler(renamed))
  
  watcher.IncludeSubdirectories <- true
  
  watcher.EnableRaisingEvents <- true

let EXTENSION_BLOCKLIST = [
  ".pdn";
  ".pdnSave";
  ".TMP";
]
let is_blocked_filetype uri =
  let filetype = Path.GetExtension uri
  if filetype = "" then false
  else
    EXTENSION_BLOCKLIST
    |> List.exists(fun t -> t = filetype)

let on_changed _ (e:FileSystemEventArgs) =
  if is_blocked_filetype e.FullPath then ()
  elif Directory.Exists e.FullPath then

    modify_pack_lock config.source (get_cutdown_filename e.FullPath) NodeType.Directory (DirectoryInfo(e.FullPath).LastWriteTimeUtc)
  elif e.Name <> "pack.lock" && File.Exists e.FullPath then
    get_cutdown_filename e.FullPath
    |> colorprintfn "$magenta[Updated file] %s"

    modify_pack_lock config.source (get_cutdown_filename e.FullPath) NodeType.File (FileInfo(e.FullPath).LastWriteTimeUtc)

let on_created _ (e:FileSystemEventArgs) =
  if is_blocked_filetype e.FullPath then ()
  elif Directory.Exists e.FullPath then
    e.FullPath
    |> duplicate_directory
  elif e.Name <> "pack.lock" then
    get_cutdown_filename e.FullPath
    |> duplicate_file
    

let on_deleted _ (e:FileSystemEventArgs) =
  if is_blocked_filetype e.FullPath then ()
  elif Directory.Exists (get_output_filename e.FullPath) then
    get_output_filename e.FullPath
    |> delete_directory
  elif e.Name <> "pack.lock" && File.Exists (get_output_filename e.FullPath) then
    get_cutdown_filename e.FullPath
    |> colorprintfn "$red[Deleting file]:%s"

    get_output_filename e.FullPath
    |> File.Delete

let on_renamed _ (e:RenamedEventArgs) =
  if is_blocked_filetype e.FullPath then ()
  elif Directory.Exists e.FullPath then
    colorprintfn @"$blue[Renaming directory] %s$yellow[%s] => ...%s$yellow[%s]" (e.OldName.Replace(Path.GetFileName e.OldName, "")) (Path.GetFileName e.OldName) @"\" (Path.GetFileName e.FullPath)
    Directory.Move(get_output_filename e.OldFullPath, get_output_filename e.FullPath)
  elif e.Name <> "pack.lock" then
    if Path.GetExtension e.OldName = ".pdnSave" && Path.GetExtension e.Name = ".png" then
      get_cutdown_filename e.FullPath
      |> duplicate_file
    else
      colorprintfn @"$blue[Renaming file] %s$yellow[%s] => ...%s$yellow[%s]" (e.OldName.Replace(Path.GetFileName e.OldName, "")) (Path.GetFileName e.OldName) @"\" (Path.GetFileName e.FullPath)
      File.Move(get_output_filename e.OldFullPath, get_output_filename e.FullPath)

[<EntryPoint>]
let main argv =
  printfn "[Press ctrl+c to quit]"
  let nodes = fetch_new_pack_lock config.source
  let dirs:PackLockMap = nodes |> Map.filter(fun k (n, d) -> n = NodeType.Directory)
  let files:PackLockMap = nodes |> Map.filter(fun k (n, d) -> n = NodeType.File)
  printfn "Directories: %i, Files: %i" dirs.Count files.Count

  watch_directory_for_changes config.source on_changed on_created on_deleted on_renamed

  init ()
  exitEvent.WaitOne() |> ignore
  // Clean-up here
  save_pack_lock config.source
  0
