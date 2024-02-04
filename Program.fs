open Argu

open Interpreter

type CliArguments =
    | Working_Directory of path:string
    | Scripts of path:list<string>
    | Log_Level of level:int
    
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "specify a working directory."
            | Scripts _ -> "specify a list of scripts to run"
            | Log_Level _ -> "specify a log level"

type Parameters = 
    { WorkingDirectory: string
      Scripts: list<string>
      LogLevel: int }

    static Empty =
        { WorkingDirectory = System.Environment.CurrentDirectory
          Scripts = []
          LogLevel = 0 }

let rec repl () =
    printf ">>"
    match System.Console.ReadLine() with
    | null -> ()
    | input -> 
        interpret input |> ignore
        repl ()

let updateParams parameters param =
    match param with
    | Working_Directory path -> {parameters with WorkingDirectory = path}
    | Scripts paths -> {parameters with Scripts = paths}
    | Log_Level level -> {parameters with LogLevel = level}

let run parameters =
    System.Environment.CurrentDirectory <- parameters.WorkingDirectory
    match parameters.Scripts with
    | [] -> repl ()
    | paths -> paths |> List.iter (System.IO.File.ReadAllText >> interpret >> ignore)

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<CliArguments>(programName = "lox")
        parser.ParseCommandLine(inputs = argv, raiseOnUsage = true).GetAllResults()
        |> List.fold updateParams Parameters.Empty
        |> run
    with e ->
        printfn "%s" e.Message
    0