module Cipher

open System.Text
open System.Security.Cryptography
open MathNet.Numerics.LinearAlgebra
open SBox
open GaloisField

let byteToHex bytes = 
    bytes 
    |> List.map (fun (x : byte) -> System.String.Format("{0:X2}", x))

let xor x y = x ^^^ y
let xor3 x y z = (x ^^^ y) ^^^ z

let rotWord (l : 'a list) =
    [l.[1]; l.[2]; l.[3]; l.[0]]

let makeSquare (l : 'a list) =
    [[l.[0]; l.[1]; l.[2]; l.[3]]
     [l.[4]; l.[5]; l.[6]; l.[7]]
     [l.[8]; l.[9]; l.[10]; l.[11]]
     [l.[12]; l.[13]; l.[14]; l.[15]]]

let (MC : GFMember list list)=
    [[02us; 03us; 01us; 01us]
     [01us; 02us; 03us; 01us]
     [01us; 01us; 02us; 03us]
     [03us; 01us; 01us; 02us]] |> List.map (fun x -> List.map GFMember x)

let (iMC : GFMember list list)=
    [[14us; 11us; 13us; 09us]
     [09us; 14us; 11us; 13us]
     [13us; 09us; 14us; 11us]
     [11us; 13us; 09us; 14us]] |> List.map (fun x -> List.map GFMember x)

let RCon =
    [[0x01; 0x00; 0x00; 0x00]
     [0x02; 0x00; 0x00; 0x00]
     [0x04; 0x00; 0x00; 0x00]
     [0x08; 0x00; 0x00; 0x00]
     [0x10; 0x00; 0x00; 0x00]
     [0x20; 0x00; 0x00; 0x00]
     [0x40; 0x00; 0x00; 0x00]
     [0x80; 0x00; 0x00; 0x00]
     [0x1b; 0x00; 0x00; 0x00]
     [0x36; 0x00; 0x00; 0x00]] |> List.map (fun x -> List.map byte x) 

let hashPassword (s : string) =
    use MD = MD5.Create()
    let result = MD.ComputeHash (Encoding.UTF8.GetBytes s)|> Array.toList |> makeSquare
    MD.Dispose()
    result

let tBytesSplit128 (s : byte[]) =
    let rec supplementList (lst : byte list)=
        let l = lst.Length
        match (l % 16) with
        |0 -> lst
        |_ -> List.append lst (List.singleton 0uy) |> supplementList
    s |> Array.toList |> supplementList |> List.chunkBySize 16

let rec keyExpansion (keySchedule : byte list list list) (rcon : byte list list) =
    match keySchedule.Length with
    |11 -> keySchedule |> List.map (fun x -> List.concat x)
    |_ ->
        let work_key = keySchedule.[keySchedule.Length - 1]
        let work_rcon :: tail_rcon = rcon 
        let nth_col n =
            [work_key.[0].[n]; work_key.[1].[n]; work_key.[2].[n]; work_key.[3].[n]]
        let w_last_column =
            nth_col 3
            |> rotWord
            |> List.map cachedSBox
        let new_first_col = List.map3 xor3 (nth_col 0) w_last_column work_rcon
        let new_second_col = List.map2 xor new_first_col (nth_col 1)
        let new_third_col = List.map2 xor new_second_col (nth_col 2)
        let new_last_col = List.map2 xor new_third_col (nth_col 3)
        let new_nth_row n = [new_first_col.[n]; new_second_col.[n]; new_third_col.[n]; new_last_col.[n]]
        let new_keySchedule = 
            [new_nth_row 0; new_nth_row 1; new_nth_row 2; new_nth_row 3]
             |> List.singleton
             |> List.append keySchedule
        keyExpansion new_keySchedule tail_rcon

let mixColA (col : GFMember list) =
    let nth_el n =
        let w = MC.[n]
        w.[0]*col.[0] + w.[1]*col.[1] + w.[2]*col.[2] + w.[3]*col.[3]
    [nth_el 0; nth_el 1; nth_el 2; nth_el 3]

let invMixColA (col : GFMember list) =
    let nth_el n =
        let w = iMC.[n]
        w.[0]*col.[0] + w.[1]*col.[1] + w.[2]*col.[2] + w.[3]*col.[3]
    [nth_el 0; nth_el 1; nth_el 2; nth_el 3]

let addRoundKey (key : byte list) (state : byte list)  =
    List.map2 xor state key

let subBytes (state : byte list) =
    List.map cachedSBox state

let invSubBytes (state : byte list) =
    List.map cachedInvSBox state

let shiftRows (state : byte list) =
    [[state.[0]; state.[1]; state.[2]; state.[3]]; [state.[5]; state.[6]; state.[7]; state.[4]]; [state.[10]; state.[11]; state.[8]; state.[9]]; [state.[15]; state.[12]; state.[13]; state.[14]]]
    |> List.map (fun x -> List.map (fun y -> GFMember(uint16 y)) x)

let invShiftRows (state : byte list) =
    [state.[0]; state.[1]; state.[2]; state.[3]; state.[7]; state.[4]; state.[5]; state.[6]; state.[10]; state.[11]; state.[8]; state.[9]; state.[13]; state.[14]; state.[15]; state.[12]]
    
let mixColumns (state : GFMember list list) =
    let n_state = List.transpose state
    List.map mixColA n_state
    |> List.map (fun (x : GFMember list) -> List.map (fun (y : GFMember) -> y.value |> byte) x)
    |> List.transpose
    |> List.concat

let invMixColumns (state : byte list) =
    let n_state =
        state
        |> makeSquare
        |> List.map (fun x -> List.map (fun y -> GFMember(uint16 y)) x)
        |> List.transpose
    List.map invMixColA n_state
    |> List.map (fun (x : GFMember list) -> List.map (fun (y : GFMember) -> y.value |> byte) x)
    |> List.transpose
    |> List.concat 
     
let crypt (keyEx : byte list list) (state : byte list)  =
    let initial_key :: main_keyEx = keyEx
    let initial_round = addRoundKey initial_key state 
    let rec main_rounds count (st : byte list) (kS : byte list list) =
        match count with
        |10 ->
            let round_key :: [] = kS
            let new_st = 
                st
                |> subBytes
                |> shiftRows
                |> List.map (fun (x : GFMember list) -> List.map (fun (y : GFMember) -> y.value |> byte) x)
                |> List.concat
                |> addRoundKey round_key
            new_st
    
        |_ ->
            let new_count = count + 1
            let round_key :: new_kS = kS
            let new_st =
                st
                |> subBytes
                |> shiftRows
                |> mixColumns
                |> addRoundKey round_key
            main_rounds new_count new_st new_kS
    main_rounds 1 initial_round main_keyEx               

let decrypt (keyEx : byte list list) (state : byte list)  =
    let wKeyEx = List.rev keyEx
    let initial_key :: main_keyEx = wKeyEx
    let initial_round = addRoundKey initial_key state |> invShiftRows |> invSubBytes
    let rec main_rounds count (st : byte list) (kS : byte list list) =
        match count with
        |10 ->
            let round_key :: [] = kS
            let new_st = 
                st
                |> addRoundKey round_key
            new_st
    
        |_ ->
            let new_count = count + 1
            let round_key :: new_kS = kS
            let new_st =
                st
                |> addRoundKey round_key
                |> invMixColumns
                |> invShiftRows
                |> invSubBytes 
            main_rounds new_count new_st new_kS
    main_rounds 1 initial_round main_keyEx  

let cmp_f_md5 file1 file2 =
    use MD = MD5.Create()
    use stream1 = System.IO.File.OpenRead file1
    use stream2 = System.IO.File.OpenRead file2
    let hash1 = MD.ComputeHash stream1
    let hash2 = MD.ComputeHash stream2
    let s_hash1 = System.BitConverter.ToString(hash1).Replace("-", "").ToLowerInvariant()
    let s_hash2 = System.BitConverter.ToString(hash2).Replace("-", "").ToLowerInvariant()
    System.Console.WriteLine("{0} MD5: {1}", file1, s_hash1)
    System.Console.WriteLine("{0} MD5: {1}", file2, s_hash2)
    System.Console.WriteLine(if s_hash1.Equals(s_hash2) then "Files are equal!" else  "Files aren't equal!")

