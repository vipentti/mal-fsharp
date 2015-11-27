namespace MAL

module Types = 

    type ParseError (s) =
        inherit System.Exception(sprintf "%s" s)

    type MalType =
        | Atom of string
        | List of list<MalType>
        | Bool of bool
        | String of string
        | Number of int

