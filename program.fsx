open System
open System.Text
open System.IO

let calculateHash (input:string) =
    let md5 = System.Security.Cryptography.MD5.Create() 
    let hash =  System.Text.Encoding.ASCII.GetBytes(input) |> md5.ComputeHash
    let sb = System.Text.StringBuilder()

    hash |> Array.iter (fun i -> (i.ToString("X2") |> sb.Append |> ignore)) 

    sb.ToString()

let checkHash (value : string[]) (hash : string) =
    let me = String.Concat(value)
    let myHash = calculateHash me
    myHash.ToLower() = hash.ToLower()

let alphabetize (value:string) = 
    String.Concat(value.ToCharArray() 
    |> Array.sort  
    |> Array.map (fun i -> string i))
let stringItem (value:string) = value, value.Length
let areAlphaEquale value1 value2 = (alphabetize value1 = alphabetize value2)
let hasUnusedChars (focus : string) (valueToCheck : string)=
    valueToCheck.ToCharArray() |> Array.exists (fun item ->  
        //printfn "item: %c, focus.IndexOf(item): %d" item (focus.IndexOf(item))
        focus.IndexOf(item) = -1
    )

let loadWordsFromFile filename input = 
    System.IO.Path.Combine(System.Environment.CurrentDirectory, filename) 
    |> System.IO.File.ReadAllLines
    //|> Array.take 5000
    |> Array.filter (fun i -> i.Length > 1)
    |> Array.filter ((fun i -> hasUnusedChars input i) >> not)
    |> Array.sortByDescending (fun i -> i.Length)
    |> Array.map stringItem
let loadWords input = loadWordsFromFile "wordlist.txt" input

let printWord (word:string*int) =
    match word with
    | value, length -> printfn "| %s | %d |" value length
let printWords (words : (string*int)[]) = words |> Array.iter (fun i -> printWord i) 

let printCandidate (candidate : string[]) =
    candidate |> Array.iter (fun i -> printf "| %s " i)
    printfn "|"

let printCandidates (candidates : System.Collections.Generic.IEnumerable<string[]>) =  
    for item in candidates do
        printCandidate item


let rec uberCandidatesRec (input : string) (lengthLeft : int) (runningSet : string[]) (candidates : System.Collections.Generic.List<string[]>) (orderedPool : (string * int)[]) =

    orderedPool |> Array.Parallel.iteri (fun i item ->
        let itemValue, itemLength = item
        let itemLengthLeft = lengthLeft - itemLength
        let set = Array.append runningSet [|itemValue|]
        if itemLengthLeft > 0 && i < orderedPool.Length - 1 then 
            uberCandidatesRec input itemLengthLeft set candidates (Array.sub orderedPool (i + 1) (orderedPool.Length - i - 1)) 
        elif itemLengthLeft = 0 && alphabetize(String.Concat(set)) = alphabetize(input) then
            candidates.Add set
            printCandidate set  
    )
let ThreeSumCandidates (orderedPool : (string * int)[]) (input : string) =
    let candidates = System.Collections.Generic.List<string[]>()

    if input.Length % 3 <> 0 then printfn "There will be errors." 
    let substract = input.Length / 3
    let list = orderedPool |> Array.map ( fun item ->
        let itemV, itemC = item
        (itemV, itemC - substract))

    for i in 0 .. list.Length - 3 do
        let aValue, aLength = list.[i]
        let mutable start = i + 1
        let mutable finnish = list.Length - 1
        while start < finnish do    
            let bValue, bLength = list.[start]  
            let cValue, cLength = list.[finnish]
            let set = [|aValue; bValue; cValue|]
            printf "i: %d, start: %d, finnish: %d, a+b+c: %d," i start finnish (aLength + bLength + cLength)
            printCandidate set
            if (aLength + bLength + cLength = 0) then
                if alphabetize(String.Concat(set)) = alphabetize(input) then 
                    candidates.Add set
                    printfn "BINGO!" //printCandidate set
                start <- start + 1
                finnish <- finnish - 1
            elif (aLength + bLength + cLength > 0) then
                finnish <- finnish - 1
            else 
                start <- start + 1
    candidates    
    //     let itemLengthLeft = lengthLeft - itemLength
    //     let set = Array.append runningSet [|itemValue|]
    //     if itemLengthLeft > 0 && i < orderedPool.Length - 1 then 
    //         uberCandidatesRec2 input itemLengthLeft set candidates (Array.sub orderedPool (i + 1) (orderedPool.Length - i - 1)) 
    //     elif itemLengthLeft = 0 && alphabetize(String.Concat(set)) = alphabetize(input) then
    //         candidates.Add set

    
let main () =
    let anagram = "poultry outwits ants" 
    let md5hash = "4624d200580677270a54ccff86b9610e" 

    let candidates = System.Collections.Generic.List<string[]>()
    let value = "poultryoutwitsants" //"poultryoutwitsants"
    let length = value.Length

    let words = loadWords value  // candidatesFromFile("ant")

    printfn "--------- List Candidates --------------"
//    words |> uberCandidatesRec value length [||] candidates 
//    candidates |> printCandidates

    let can3 = ThreeSumCandidates words value
    
    //words |> printWords  
    words |> Array.length |> printfn "Dictionary Count: %d"  

    ""

    //words |> printWords
    // let expectedHash = calculateHash anagram
//main()

        

