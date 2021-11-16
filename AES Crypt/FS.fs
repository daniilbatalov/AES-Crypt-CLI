module FS
open System.Text
open System.IO

let readFile filePath = File.ReadAllBytes (filePath)

let writeFile filePath contents = File.WriteAllBytes (filePath, contents)


