namespace MAL

module Reader = 
    open System.Text.RegularExpressions
    open Types

    let PATTERN = """[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)"""
    
    type Reader(tokens : list<string>) =
        let mutable position = 0
        member this.Tokens = tokens

        member this.Peek() = 
            this.Tokens.Item position

        member this.Next() =
            let ret = this.Peek()
            position <- position + 1
            ret

        member this.IsDone() =
            position = this.Tokens.Length - 1
        

    let rec ReadStr str = 
        ReadForm (new Reader(Tokenizer str))
        

    and Tokenizer str =
        let matches = Regex.Matches(str, PATTERN)

        if matches.Count > 0 then
            [for m in matches -> m.Value.Trim()] |> List.filter (fun x -> x.Length > 0)
        else
            []

    and ReadForm (reader : Reader) = 
        match reader.Peek() with
        | "(" -> ReadList reader
        | _   -> ReadAtom reader


    and ReadList (reader : Reader) = 
        //Skip the initial (
        ignore (reader.Next())

        let mutable ret : list<MalType> = []

        while not (reader.Peek().Equals(")")) && not (reader.IsDone()) do
            let value = ReadForm reader

            ret <- ret @ [value]

        List ret
            

    and ReadAtom (reader : Reader) = 
        let value = reader.Next()

        try
            let number = System.Int32.Parse(value)
            Number number
        with
            | _ -> Atom value
            

    