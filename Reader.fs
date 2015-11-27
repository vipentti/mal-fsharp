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
        //ReadForm (new Reader(Tokenizer str))
        let reader = new Reader(Tokenizer str)

        let matches = EnsureMatch reader

        if matches then
            ReadForm reader
        else
            raise (ParseError("Missing matching parenthesis"))
        

    and Tokenizer str =
        let matches = Regex.Matches(str, PATTERN)

        //NOTE(ville): The item at index 1 in each matched group is the actual match string
        if matches.Count > 0 then
            [for m in matches -> (m.Groups.Item 1).Value.Trim()] |> List.filter (fun x -> x.Length > 0)
        else
            []

    and EnsureMatch (reader : Reader) = 
        let howMany pred = Seq.filter pred >> Seq.length

        let parensMatch = howMany (fun x -> x = "(") reader.Tokens = howMany (fun x -> x = ")") reader.Tokens

        parensMatch



    and ReadForm (reader : Reader) = 
        match reader.Peek() with
        | "("   -> ReadList reader
        | "nil" -> ReadNil reader
        | "true" | "false" -> ReadBool reader
        | _     -> ReadAtom reader


    and ReadList (reader : Reader) = 
        //Skip the initial (
        ignore (reader.Next())

        let mutable ret : list<MalType> = []

        //TODO(ville): Make this into a functional call
        while not (reader.Peek().Equals(")")) do

            if reader.IsDone() then raise (ParseError("Missing matching parenthesis"))
            
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
            

    and ReadNil (reader : Reader) =
        ignore (reader.Next())
        Nil

    and ReadBool (reader : Reader) = 
        match reader.Next() with
        | "true" | "#t" -> Bool true
        | "false" | "#f" -> Bool false
        | _ -> Bool false

    