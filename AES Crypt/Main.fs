module Main

open Cache
open GaloisField
open SBox
open FS
open Cipher

[<EntryPoint>]
let main args =
    let arg = args.[0]
    let startTime = System.Diagnostics.Stopwatch.StartNew();
    match arg with
    |"-crypt" ->
        let inPath = args.[1]
        let outPath = args.[2]
        let password = args.[3] |> hashPassword |> List.singleton
        let keySchedule = keyExpansion password RCon
        let textToWork = readFile inPath |> tBytesSplit128
        startTime.Start()
        List.map (crypt keySchedule) textToWork
        |> List.concat 
        |> List.toArray
        |> writeFile outPath
        startTime.Stop()

    |"-decrypt" ->
        let inPath = args.[1]
        let outPath = args.[2]
        let password = args.[3] |> hashPassword |> List.singleton
        let keySchedule = keyExpansion password RCon
        let textToWork = readFile inPath |> tBytesSplit128
        startTime.Start()
        List.map (decrypt keySchedule) textToWork
        |> List.concat 
        |> List.toArray
        |> writeFile outPath
        startTime.Stop()

    |"-check" ->
        let inPath = "check_in"
        let outPath = "check_out"
        let out2Path = "check_decrypted"
        let check_in = [0x32; 0x88; 0x31; 0xE0; 0x43; 0x5A; 0x31; 0x37; 0xF6; 0x30; 0x98; 0x07; 0xA8; 0x8D; 0xA2; 0x34] |> List.map byte |> List.toArray
        let check_key = [0x2B; 0x28; 0xAB; 0x09; 0x7E; 0xAE; 0xF7; 0xCF; 0x15; 0xD2; 0x15; 0x4F; 0x16; 0xA6; 0x88; 0x3C] |> List.map byte |> List.chunkBySize 4 |> List.singleton
        writeFile inPath check_in 
        let keySchedule = keyExpansion check_key RCon
        let textToWorkA = check_in |> tBytesSplit128
        startTime.Start()
        List.map (crypt keySchedule) textToWorkA
        |> List.concat 
        |> List.toArray
        |> writeFile outPath
        let textToWorkB = readFile outPath |> tBytesSplit128
        List.map (decrypt keySchedule) textToWorkB
        |> List.concat 
        |> List.toArray
        |> writeFile out2Path
        startTime.Stop()
        cmp_f_md5 inPath out2Path

    |_ ->
        raise(System.ArgumentException("Incorrect argument!"))
    if ((args.Length = 5) && (Array.contains "-benchmark" args)) then 
        System.Console.WriteLine("Elapsed time: {0:00}:{1:00}:{2:00}.{3:00}", startTime.Elapsed.Hours, startTime.Elapsed.Minutes, startTime.Elapsed.Seconds, startTime.Elapsed.Milliseconds / 10)
    0
