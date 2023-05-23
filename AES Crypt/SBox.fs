module SBox
open GaloisField
open Cache
open MathNet.Numerics.LinearAlgebra



let SBoxTMatrix = DenseMatrix.ofRowList   [[ 1.0; 0.0; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0 ];
                                           [ 1.0; 1.0; 0.0; 0.0; 0.0; 1.0; 1.0; 1.0 ];
                                           [ 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 1.0; 1.0 ];
                                           [ 1.0; 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 1.0 ];
                                           [ 1.0; 1.0; 1.0; 1.0; 1.0; 0.0; 0.0; 0.0 ];
                                           [ 0.0; 1.0; 1.0; 1.0; 1.0; 1.0; 0.0; 0.0 ];
                                           [ 0.0; 0.0; 1.0; 1.0; 1.0; 1.0; 1.0; 0.0 ];
                                           [ 0.0; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0; 1.0 ]]

let InvSBoxTMatrix = DenseMatrix.ofRowList [[ 0.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 1.0 ];
                                            [ 1.0; 0.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0 ];
                                            [ 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 0.0; 1.0 ];
                                            [ 1.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 0.0 ];
                                            [ 0.0; 1.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0 ];
                                            [ 0.0; 0.0; 1.0; 0.0; 1.0; 0.0; 0.0; 1.0 ];
                                            [ 1.0; 0.0; 0.0; 1.0; 0.0; 1.0; 0.0; 0.0 ];
                                            [ 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 1.0; 0.0 ]]

let SBoxConstVector = DenseMatrix.ofColumnList [[1.0; 1.0; 0.0; 0.0; 0.0; 1.0; 1.0; 0.0]]

let InvSBoxConstVector = DenseMatrix.ofColumnList [[1.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0]]

let rec suppl (bL : seq<float>) =
    match (Seq.length bL) with
    |8 -> bL
    |_ -> Seq.append bL (Seq.singleton 0.0) |> suppl

let intFunc (b : byte) = 
    System.Convert.ToString(b, 2)
    |> Seq.map (fun x -> string x |> float)
    |> Seq.rev
    |> suppl
    |> Seq.singleton
    |> DenseMatrix.ofColumnSeq

let afterTrns a =
    a
    |> Array.exactlyOne
    |> Array.map (fun x -> (int x) % 2 |> string)
    |> Array.rev
    |> Array.append [|"0b"|]
    |> Array.fold (fun acc x -> acc + x) ""
    |> byte

let SBox (b : byte) =
    let invBMatrix =
        let wB = invByte b
        intFunc wB
        |> ( * ) SBoxTMatrix
        |> ( + ) SBoxConstVector
    invBMatrix.ToColumnArrays() |> afterTrns

let InvSBox (b : byte) =
    let invBMatrix =
        intFunc b
        |> ( * ) InvSBoxTMatrix
        |> ( + ) InvSBoxConstVector
    invBMatrix.ToColumnArrays() |> afterTrns |> invByte

let cachedSBox = cache SBox
let cachedInvSBox = cache InvSBox
    
    
