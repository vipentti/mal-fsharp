module Types 

    type ParseError (s) =
        inherit System.Exception(sprintf "%s" s)

    [<CustomEquality;CustomComparison>]
    type MalType =
        | Nil
        | List of MetaData * list<MalType>
        | Vector of MetaData * list<MalType>
        | Bool of bool
        | Symbol of string
        | String of string
        | Keyword of string
        | Number of int
        | HashMap of MetaData * Collections.Map<MalType, MalType>
        | PrimitiveFunction of MetaData * int * (list<MalType> -> MalType)
        | Function of MetaData * int * (list<MalType> -> MalType) * MalType * list<MalType> * EnvChain 
        | Macro of MetaData * int * (list<MalType> -> MalType) * MalType * list<MalType> * EnvChain 
        | Atom of int * MalType Ref

        static member private hashSeq (s : seq<MalType>) =
            let iter st node = (st * 397) ^^^ node.GetHashCode()
            s |> Seq.fold iter 0

        static member private allEqual (x : seq<MalType>) (y : seq<MalType>) =
            use ex = x.GetEnumerator()
            use ey = y.GetEnumerator()
            let rec loop () =
                match ex.MoveNext(), ey.MoveNext() with
                | false, false -> true
                | false, true
                | true, false -> false
                | true, true ->
                    if ex.Current = ey.Current then
                        loop ()
                    else
                        false
            loop ()

        static member private allCompare (x : seq<MalType>) (y : seq<MalType>) =
            use ex = x.GetEnumerator()
            use ey = y.GetEnumerator()
            let rec loop () =
                match ex.MoveNext(), ey.MoveNext() with
                | false, false -> 0
                | false, true -> -1
                | true, false -> 1
                | true, true ->
                    let cmp = compare ex.Current ey.Current
                    if cmp = 0 then loop () else cmp
            loop ()

        static member private rank x =
            match x with
            | Nil -> 0
            | List(_) -> 1
            | Vector(_) -> 2
            | HashMap( _) -> 3
            | Symbol(_) -> 4
            | Keyword(_) -> 5
            | Number(_) -> 6
            | String(_) -> 7
            | Bool(_) -> 8
            | PrimitiveFunction(_) -> 9
            | Function(_) -> 9
            | Macro(_) -> 9
            | Atom(_, _) -> 10

        static member private equals x y =
            match x, y with
            | Nil, Nil -> true
            | List(_, a), List(_, b) -> a = b
            | List(_, a), Vector(_, b) -> MalType.allEqual a b
            | Vector(_, a), List(_, b) -> MalType.allEqual a b
            | Vector(_, a), Vector(_, b) -> MalType.allEqual a b
            | HashMap(_, a), HashMap(_, b) -> a = b
            | Symbol(a), Symbol(b) -> a = b
            | Keyword(a), Keyword(b) -> a = b
            | Number(a), Number(b) -> a = b
            | String(a), String(b) -> a = b
            | Bool(a), Bool(b) -> a = b
            | (PrimitiveFunction(_, a, _) | Function (_, a, _, _, _, _) | Macro (_, a, _, _, _, _)), 
              (PrimitiveFunction(_, b, _) | Function (_, b, _, _, _, _) | Macro (_, b, _, _, _, _)) -> 
                a = b
            | Atom(a, _), Atom(b, _) -> a = b
            | _, _ -> false

        static member private compare x y =
            match x, y with
            | Nil, Nil -> 0
            | List(_, a), List(_, b) -> compare a b
            | List(_, a), Vector(_, b) -> MalType.allCompare a b
            | Vector(_, a), List(_, b) -> MalType.allCompare a b
            | Vector(_, a), Vector(_, b) -> MalType.allCompare a b
            | HashMap(_, a), HashMap(_, b) -> compare a b
            | Symbol(a), Symbol(b) -> compare a b
            | Keyword(a), Keyword(b) -> compare a b
            | Number(a), Number(b) -> compare a b
            | String(a), String(b) -> compare a b
            | Bool(a), Bool(b) -> compare a b
            | (PrimitiveFunction(_, a, _) | Function (_, a, _, _, _, _) | Macro(_, a, _, _ ,_ ,_)), 
              (PrimitiveFunction(_, b, _) | Function (_, b, _, _, _, _) | Macro(_, b, _, _ ,_ ,_)) -> 
                compare a b
            | Atom(a, _), Atom(b, _) -> compare a b
            | a, b -> compare (MalType.rank a) (MalType.rank b)


        override x.Equals yobj =
            match yobj with
            | :? MalType as y -> MalType.equals x y
            | _ -> false

        override x.GetHashCode() =
            match x with
            | Nil -> 0
            | List(_, lst) -> hash lst
            | Vector(_, vec) -> MalType.hashSeq vec
            | HashMap(_, map) -> hash map
            | Symbol(sym) -> hash sym
            | Keyword(key) -> hash key
            | Number(num) -> hash num
            | String(str) -> hash str
            | Bool(b) -> hash b
            | PrimitiveFunction(_, tag, _) | Function(_, tag, _, _, _, _) | Macro(_, tag, _, _, _, _) ->
                hash tag
            | Atom(tag, _) -> hash tag

        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? MalType as y -> MalType.compare x y
                | _ -> invalidArg "yobj" "Cannot compare values of different types."


    and Env = System.Collections.Generic.Dictionary<string, MalType>
    and EnvChain = Env list
    and MetaData = MalType

    exception MalException of MalType


    let makeHashMap s = HashMap(Nil, s)
    let makeList s = List(Nil, s)
    let makeVector s = Vector(Nil, s)

