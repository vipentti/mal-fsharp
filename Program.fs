namespace MAL

module Main = 
    open System
    open Reader
    open Printer

    let READ str =
        Reader.ReadStr str

    let EVAL ast env =
        ast

    let PRINT exp =
        Printer.PrStr exp

    let REP str =
        str |> READ |> (fun x -> EVAL x "") |> PRINT

    [<EntryPoint>]
    let main argv = 
        //printfn "%A" argv

        let mutable running = true

        while running do
            Console.Write("user> ")
            let line = Console.ReadLine()

            if line = null || line = "quit" then
                running <- false
            else
                //Console.WriteLine(REP line)
                try 
                    let result = REP line
                    Console.WriteLine(result)
                with
                    | ex -> printfn "Error: %A" ex

        0 // return an integer exit code
