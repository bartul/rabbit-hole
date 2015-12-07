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
//    |> Array.take 100
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
 
 
// TODO: Do it rec asap, stupid
let uberCandidates (input : string) (orderedPool : (string * int)[]) =
    let mutable candidates = System.Collections.Generic.List<string[]>()
    let length = input.Length
    
    for i in 0 .. orderedPool.Length - 1 do

        let first = orderedPool.[i]
        let firstValue, firstLength = first
        let firstLengthLeft = length - firstLength
        if firstLengthLeft = 0 && alphabetize(firstValue) = alphabetize(input) then candidates.Add [|firstValue|] 

        if firstLengthLeft > 0 then 
            for j in i + 1 .. orderedPool.Length - 1 do

                let second = orderedPool.[j]
                let secondValue, secondLength = second
                let secondLengthLeft = firstLengthLeft - secondLength
                if secondLengthLeft = 0 && alphabetize(String.Concat(firstValue, secondValue)) = alphabetize(input) then candidates.Add [|firstValue; secondValue|] 
                
                if secondLengthLeft > 0 then 
                    for k in j + 1 .. orderedPool.Length - 1 do
                        
                        let third = orderedPool.[k]
                        let thirdValue, thirdLength = third
                        let thirdLengthLeft = secondLengthLeft - thirdLength
                        if thirdLengthLeft = 0 && alphabetize(String.Concat(firstValue, secondValue, thirdValue)) = alphabetize(input) then candidates.Add [|firstValue; secondValue; thirdValue|] 


        //printWord first
    candidates
let printCandidates (candidates : System.Collections.Generic.IEnumerable<string[]>) =
    for item in candidates do
        item |> Array.iter (fun i -> printf "| %s " i)
        printfn "|"

let main () =
    let anagram = "poultry outwits ants" 
    let md5hash = "4624d200580677270a54ccff86b9610e" 

    let words = loadWords() // candidatesFromFile("ant")
    words |> uberCandidates "poultryoutwitsants" |> printCandidates
    
    ""

    //words |> printWords
    // let expectedHash = calculateHash anagram
main()

        

