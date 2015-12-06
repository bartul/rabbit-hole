open System
open System.Text
open System.IO

let calculateHash (input:string) =
    let md5 = System.Security.Cryptography.MD5.Create() 
    let hash =  System.Text.Encoding.ASCII.GetBytes(input) |> md5.ComputeHash
    let sb = System.Text.StringBuilder()

    hash |> Array.iter (fun i -> (i.ToString("X2") |> sb.Append |> ignore)) 

    sb.ToString()

let stringItem (value:string) = 
        value, value.Length, String.Concat(value.ToCharArray() 
                            |> Array.sort  
                            |> Array.map (fun i -> string i))

let candidates (pool:(string*int*string)[], origin:string) =
    let value, length, sorted = stringItem origin
    pool 
    |> Array.filter (fun i ->
        match i with
        | v, _, s when v <> value -> s = sorted
        | _ -> false  
        )
let loadWords () = 
    System.IO.Path.Combine(System.Environment.CurrentDirectory, "wordlist.txt") 
    |> System.IO.File.ReadAllLines 
    |> Array.map stringItem
let candidatesFromFile value = candidates (loadWords(), value) 

let printWords (words:(string*int*string)[]) =
    words
    //|> Array.take 20
    |> Array.iter (fun i -> 
        match i with
        | (value, length, sorted) -> printfn "| %s | %d | %s |" value length sorted)

let main () =
    let anagram = "poultry outwits ants" 
    let md5hash = "4624d200580677270a54ccff86b9610e" 

    let words = candidatesFromFile("ant")
    words |> printWords
    // let expectedHash = calculateHash anagram


        

