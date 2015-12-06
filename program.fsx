open System
open System.Text
open System.IO

let calculateHash (input:string) =
    let md5 = System.Security.Cryptography.MD5.Create() 
    let hash =  System.Text.Encoding.ASCII.GetBytes(input) |> md5.ComputeHash
    let sb = System.Text.StringBuilder()

    hash |> Array.iter (fun i -> (i.ToString("X2") |> sb.Append |> ignore)) 

    sb.ToString()

let alphabetize (value:string) = 
    String.Concat(value.ToCharArray() 
    |> Array.sort  
    |> Array.map (fun i -> string i))
let stringItem (value:string) = value, value.Length
let areAlphaEquale value1 value2 = alphabetize value1 = alphabetize value2
    
let loadWordsFromFile filename = 
    System.IO.Path.Combine(System.Environment.CurrentDirectory, filename) 
    |> System.IO.File.ReadAllLines
    |> Array.take 100
    |> Array.map stringItem
    |> Array.sortByDescending (fun i ->  
        match i with
        | _, l -> l
        )        
let loadWords () = loadWordsFromFile "wordlist.txt"

let printWord (word:string*int) =
    match word with
    | value, length -> printfn "| %s | %d |" value length

let printWords (words : (string*int)[]) =
    words
    |> Array.iter (fun i -> printWord i) 
 
let uberCandidates (input : string) (orderedPool : (string * int)[]) =
    let candidates = List.Empty
 
    let length = input.Length
    for i in 0 .. orderedPool.Length - 1 do
        let falco = orderedPool.[i]
        printWord falco
    candidates
let main () =
    let anagram = "poultry outwits ants" 
    let md5hash = "4624d200580677270a54ccff86b9610e" 

    let words = loadWords() // candidatesFromFile("ant")
    words |> uberCandidates "sda" |> ignore
    
    ""

    //words |> printWords
    // let expectedHash = calculateHash anagram


        

