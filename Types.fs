namespace MAL

module Types = 

    type ParseError (s) =
        inherit System.Exception(sprintf "%s" s)

    [<CustomEquality;CustomComparison>]
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
        | PrimitiveFunction of string * (list<MalType> -> MalType)
//        | Atom of string


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
            | Vector( _) -> 2
            | HashMap( _) -> 3
            | Symbol(_) -> 4
            | Keyword(_) -> 5
            | Number(_) -> 6
            | String(_) -> 7
            | Bool(_) -> 8
            | PrimitiveFunction(_, _) -> 9
//            | Func(_, _, _, _, _, _)
//            | Macro(_, _, _, _, _, _) -> 9
//            | Atom(_, _) -> 10

        static member private equals x y =
            match x, y with
            | Nil, Nil -> true
            | List(a), List(b) -> a = b
            | List(a), Vector(b) -> MalType.allEqual a b
            | Vector(a), List(b) -> MalType.allEqual a b
            | Vector(a), Vector(b) -> MalType.allEqual a b
            | HashMap(a), HashMap(b) -> a = b
            | Symbol(a), Symbol(b) -> a = b
            | Keyword(a), Keyword(b) -> a = b
            | Number(a), Number(b) -> a = b
            | String(a), String(b) -> a = b
            | Bool(a), Bool(b) -> a = b
//            | (BuiltInFunc(_, a, _) | Func(_, a, _, _, _, _) | Macro(_, a, _, _, _, _)),
//              (BuiltInFunc(_, b, _) | Func(_, b, _, _, _, _) | Macro(_, b, _, _, _, _)) ->
//                a = b
//            | Atom(a, _), Atom(b, _) -> a = b
            | PrimitiveFunction(a, _), PrimitiveFunction(b, _) -> a = b
            | _, _ -> false

        static member private compare x y =
            match x, y with
            | Nil, Nil -> 0
            | List(a), List(b) -> compare a b
            | List(a), Vector(b) -> MalType.allCompare a b
            | Vector(a), List(b) -> MalType.allCompare a b
            | Vector(a), Vector(b) -> MalType.allCompare a b
            | HashMap(a), HashMap(b) -> compare a b
            | Symbol(a), Symbol(b) -> compare a b
            | Keyword(a), Keyword(b) -> compare a b
            | Number(a), Number(b) -> compare a b
            | String(a), String(b) -> compare a b
            | Bool(a), Bool(b) -> compare a b
            | PrimitiveFunction(a, _), PrimitiveFunction(b, _) -> compare a b
//            | (BuiltInFunc(_, a, _) | Func(_, a, _, _, _, _) | Macro(_, a, _, _, _, _)),
//              (BuiltInFunc(_, b, _) | Func(_, b, _, _, _, _) | Macro(_, b, _, _, _, _)) ->
//                compare a b
//            | Atom(a, _), Atom(b, _) -> compare a b
            | a, b -> compare (MalType.rank a) (MalType.rank b)


        override x.Equals yobj =
            match yobj with
            | :? MalType as y -> MalType.equals x y
            | _ -> false

        override x.GetHashCode() =
            match x with
            | Nil -> 0
            | List(lst) -> hash lst
            | Vector(vec) -> MalType.hashSeq vec
            | HashMap(map) -> hash map
            | Symbol(sym) -> hash sym
            | Keyword(key) -> hash key
            | Number(num) -> hash num
            | String(str) -> hash str
            | Bool(b) -> hash b
            | PrimitiveFunction(tag, _) -> //| Func(_, tag, _, _, _, _) | Macro(_, tag, _, _, _, _) ->
                hash tag
//            | Atom(tag, _) -> hash tag

        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? MalType as y -> MalType.compare x y
                | _ -> invalidArg "yobj" "Cannot compare values of different types."

