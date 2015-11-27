namespace MAL

module Types = 

    type ParseError (s) =
        inherit System.Exception(sprintf "%s" s)

    type MalType =
        | Nil
        | List of list<MalType>
        | Vector of list<MalType>
        | Bool of bool
        | Symbol of string
        | String of string
        | Keyword of string
        | Number of int
        | HashMap of Collections.Map<MalType, MalType>

