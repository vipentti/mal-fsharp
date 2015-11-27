module Reader 
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

    let makeParseError item = 
        "expected '" + item + "', got EOF"

    let stripCharacters chars = String.collect (fun c -> if Seq.exists((=)c) chars then "" else string c)

    let rec ReadStr str = 
        //ReadForm (new Reader(Tokenizer str))
        let reader = new Reader(Tokenizer str)

        let (matches, error) = EnsureMatch reader

        if matches then
            ReadForm reader
        else
            raise (ParseError(error))
        

    and Tokenizer str =
        let nrQuotes s = 
            s 
            |> stripCharacters "\\\""
            |> Seq.filter (fun x -> x = '"')
            |> Seq.length

        if (nrQuotes str) % 2 <> 0 then
            raise (ParseError(makeParseError "\""))
        else
            let matches = Regex.Matches(str, PATTERN)

            //NOTE(ville): The item at index 1 in each matched group is the actual match string
            if matches.Count > 0 then
                [for m in matches -> (m.Groups.Item 1).Value.Trim()] |> List.filter (fun x -> x.Length > 0)
            else
                []

    and EnsureMatch (reader : Reader) = 
        let howMany pred = Seq.filter pred >> Seq.length

        let pairMatch (fs, sn) = 
            howMany (fun x -> x = fs) reader.Tokens = howMany (fun x -> x = sn) reader.Tokens


        let getResult (op, cl) =
            let m1 = howMany (fun x -> x = op) reader.Tokens
            let m2 = howMany (fun x -> x = cl) reader.Tokens

            if m1 = m2 then
                (true, "")
            else
                if m1 > m2 then
                    (false, makeParseError cl)
                else
                    (false, makeParseError op)


        let symbolPairs = [
            ("(", ")");
            ("[", "]");
            ("{", "}");
        ]

        let res = 
            symbolPairs 
            |> Seq.map getResult
            |> Seq.filter (fun (a, _ ) -> not a)
            |> Seq.map snd
            |> String.concat "\n"

        if res.Length = 0 then
            (true, "")
        else
            (false, res)


    and ReadForm (reader : Reader) = 
        match reader.Peek() with
        | "(" -> ReadList reader
        | "[" -> ReadVector reader
        | "{" -> ReadHashMap reader
        | "'" | "`" | "~" | "~@" -> ReadMacro reader
        | _   -> ReadAtom reader


    and ReadMacro (reader : Reader) = 
        let macro = reader.Next()

        match macro with 
        | "'" -> List ((Symbol "quote") :: [ReadForm reader])
        | "`" -> List ((Symbol "quasiquote") :: [ReadForm reader])
        | "~" -> List ((Symbol "unquote") :: [ReadForm reader])
        | "~@" -> List ((Symbol "splice-unquote") :: [ReadForm reader])
        | _ -> Nil
        

    and ReadUntil (reader : Reader) endChar lst = 
        match reader.Peek() with
        | x when x.Equals(endChar) -> 
            ignore (reader.Next())
            lst
        | _ -> ReadUntil reader endChar (lst @ [ReadForm reader])

    and splitListToPairs lst = 

        let  splitList op lst =
            lst 
            |> Seq.mapi (fun i el -> el, i)
            |> Seq.filter (fun (el, i) -> op (i % 2) 0)
            |> Seq.map fst
            |> Seq.toList

        let takeEvenIndices = 
            splitList (=)

        let takeOddIndices = 
            splitList (<>)
        
        let keys = takeEvenIndices lst
        let vals = takeOddIndices lst

        List.zip keys vals
    

    and ReadHashMap (reader : Reader) = 
        ignore (reader.Next())

        let values = ReadUntil reader "}" []

        let result = 
            //List.zip keys vals
            splitListToPairs values
            |> Map.ofList

        HashMap result

    and ReadList (reader : Reader) = 
        //Skip the initial (
        ignore (reader.Next())

        List (ReadUntil reader ")" [])

    and ReadVector (reader : Reader) = 
        ignore (reader.Next())
        Vector (ReadUntil reader "]" [])

    and ReadAtom (reader : Reader) = 
        let value = reader.Next()

        match value with
        | "nil" -> Nil
        | "true" -> Bool true
        | "false" -> Bool false
        | str when value.StartsWith("\"") -> String (str.Substring(1, str.Length - 2).Replace("\\\"", "\"").Replace("\\n", "\n").Replace("\\\\", "\\"))
        | kw when value.StartsWith(":") -> Keyword ("\xff" + kw.Substring(1))
        | _ ->
            try
                let number = System.Int32.Parse(value)
                Number number
            with
                | _ -> Symbol value
