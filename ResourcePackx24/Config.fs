module Config

open System
open System.IO
open FSharp.Json

let CONFIG_URI = "config.json"

type InputValidation =
  Float | DirectoryPath | Bool

let rec fetch_input text (validation:InputValidation) =
  printfn "%s" text
  printf "> "
  match validation with
  | InputValidation.DirectoryPath ->
    let input = Console.ReadLine()
    if Directory.Exists input then input
    else
      printfn "(!) Directory does not exist"
      fetch_input text validation
  | InputValidation.Float ->
    let input = Console.ReadLine()
    if input |> Seq.forall Char.IsDigit then
      if input |> int > 0 then input
      else
        printfn "(!) Number must be greater than 0"
        fetch_input text validation
    else
      printfn "(!) '%s' is not a valid number" input
      fetch_input text validation
  | InputValidation.Bool ->
    let input = Console.ReadKey true
    if input.Key = ConsoleKey.Y then
      printfn "Config will be used without confirmation on launch."
      "true"
    else 
      printfn "Will be asked to confirm config on launch."
      "false"
      
type Config = {
  skip_check:bool;
  source:string;
  desintation:string;
  output_size:float;
}

let to_bool str = if str = "true" then true else false

let input_config ():Config =
  let cfg = {
    source=fetch_input "Directory of the x24 pack" InputValidation.DirectoryPath;
    desintation=fetch_input "Directory for output pack" InputValidation.DirectoryPath;
    output_size=fetch_input "Output size" InputValidation.Float |> float;
    skip_check=fetch_input "Lock in these settings (y/n). Edit config.json to undo." InputValidation.Bool |> to_bool;
  }
  File.WriteAllText(CONFIG_URI, Json.serialize cfg)
  cfg

let config:Config =
  if File.Exists CONFIG_URI then
    let cfg = File.ReadAllText CONFIG_URI |> Json.deserialize<Config>
    if cfg.skip_check then cfg
    else
      printfn "Previous settings found \n%A\nWould you like to use them? (y/n)" cfg
      if System.Console.ReadKey(true).Key = ConsoleKey.Y then cfg
      else
        input_config()
  else
    input_config ()

