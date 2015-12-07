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
    //|> Array.take 5000
    |> Array.filter (fun i -> i.Length > 2)
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
 
 
let rec uberCandidatesRec (input : string) (lengthLeft : int) (cursor : int) (runningSet : string[]) (candidates : System.Collections.Generic.List<string[]>) (orderedPool : (string * int)[]) =

    for i in cursor .. orderedPool.Length - 1 do
        let item = orderedPool.[i]
        let itemValue, itemLength = item
        let itemLengthLeft = lengthLeft - itemLength
        let set = Array.append runningSet [|itemValue|]
        if itemLengthLeft > 0 && i < orderedPool.Length - 1 then 
            uberCandidatesRec input itemLengthLeft (i + 1) set candidates orderedPool 
        elif itemLengthLeft = 0 && alphabetize(String.Concat(set)) = alphabetize(input) then
            candidates.Add set

let printCandidates (candidates : System.Collections.Generic.IEnumerable<string[]>) =
    for item in candidates do
        item |> Array.iter (fun i -> printf "| %s " i)
        printfn "|"

let main () =
    let anagram = "poultry outwits ants" 
    let md5hash = "4624d200580677270a54ccff86b9610e" 

    let candidates = System.Collections.Generic.List<string[]>()
    let value = "stopsita"
    let length = value.Length

    let words = loadWords() // candidatesFromFile("ant")
    words |> uberCandidatesRec value length 0 [||] candidates 
    candidates |> printCandidates
    
//    words |> printWords  

    ""

    //words |> printWords
    // let expectedHash = calculateHash anagram
main()

        

