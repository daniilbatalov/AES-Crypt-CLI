module GaloisField

let xTimes (b : uint16) =
    let shl = (b <<< 1) &&& 255us
    match b &&& 128us with
    |0us -> shl
    |_ -> shl ^^^ 27us

let rec powMul (exp : int) (acc : uint16) =
    match exp with
    |0 -> acc
    |_ ->
        let new_exp = exp - 1
        let new_acc = xTimes (acc)
        powMul new_exp new_acc

let byteToPolyExp (b : uint16) =
    let filterPredicate (x : int*int) =
        match x with
        |_, last -> last = 1
    let mapper (x : int*int) =
        match x with
        |first, _ -> first
    System.Convert.ToString((byte b), 2)
    |> Seq.toList
    |> List.map (fun x -> string x |> int)
    |> List.rev
    |> List.indexed
    |> List.filter filterPredicate
    |> List.map mapper
    |> List.rev

type GFMember (value : uint16) =
    member this.value = value
    static member ( + ) (l : GFMember, r : GFMember) =
         GFMember(l.value ^^^ r.value)
    static member ( - ) (l : GFMember, r : GFMember) =
         GFMember(l.value ^^^ r.value)
    static member ( * ) (l : GFMember, r : GFMember) =
        let multGF (lu : uint16) (ru : uint16) =
            let ruList = byteToPolyExp ru
            List.fold (fun acc x -> acc ^^^ (powMul x lu)) 0us ruList
        GFMember(multGF l.value r.value)
    member this.invert =
        let rec power (b : GFMember) (acc : GFMember) exp =
            match exp with
            |0 -> acc
            |_ -> 
                let new_acc = acc * b
                let new_exp = exp - 1
                power b new_acc new_exp
        power this this 253
    static member ( / ) (l : GFMember, r : GFMember) =
        l * r.invert

let invByte (b : byte) =
    let GFMb = GFMember(uint16 b)
    GFMb.invert.value |> byte


