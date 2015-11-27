namespace MAL

module Types = 

    type MalType =
        | Atom of string
        | List of list<MalType>
        | Bool of bool
        | String of string
        | Number of int

