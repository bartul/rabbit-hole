open System
open System.Text
open System.Linq
open System.IO

let calculateHash (input:string) =
    let md5 = System.Security.Cryptography.MD5.Create() 
    let hash =  System.Text.Encoding.ASCII.GetBytes(input) |> md5.ComputeHash
    let sb = System.Text.StringBuilder()

    hash |> Array.iter (fun i -> (i.ToString("X2") |> sb.Append |> ignore)) 
    sb.ToString()

let checkHash (value : string[]) (hash : string) =
    let me = System.String.Join(" ", value) // String.Concat(value)
    let myHash = calculateHash me
    myHash.ToLower() = hash.ToLower()
let printWord (word:string*int) =
    match word with
    | value, length -> printfn "| %s | %d |" value length
let printWords (words : (string*int)[]) = words |> Array.iter (fun i -> printWord i) 
let printCandidate (candidate : string[]) =
    candidate |> Array.iter (fun i -> printf "| %s " i)
    printfn "|"
let AsSentence (words : string[]) = System.String.Join(" ", words)

let printCandidates (candidates : System.Collections.Generic.IEnumerable<string[]>) =  
    for item in candidates do
        printCandidate item

let rec getPermutations (candidates : System.Collections.Generic.List<string[]>) (value : string[]) (currentRunIndexes : list<int>) = 
    if currentRunIndexes.Length < value.Length  then
        for i in 0 .. value.Length - 1 do
            //printfn "%d. Values count: %d. Indexes count: %d. Has i inside: %b" i value.Length currentRunIndexes.Length (currentRunIndexes |> List.exists(fun item -> item = i))
            if not (currentRunIndexes |> List.exists(fun item -> item = i)) then 
                let currentRunIndexes = i :: currentRunIndexes 
                if value.Length = currentRunIndexes.Length then
                    let can = currentRunIndexes |> List.map (fun item -> value.[item]) |> List.toArray
                    //can |> printCandidate 
                    candidates.Add can
                    //getPermutations candidates value (System.Collections.Generic.List<int>())
                else 
                    getPermutations candidates value currentRunIndexes


let alphabetize (value:string) = 
    String.Concat(value.ToCharArray() 
    |> Array.sort  
    |> Array.map (fun i -> string i))
let stringItem (value:string) = value, value.Length
let areAlphaEquale value1 value2 = (alphabetize value1 = alphabetize value2)
let hasUnusedChars (focus : string) (valueToCheck : string)=
    valueToCheck.ToCharArray() |> Array.exists (fun item -> focus.IndexOf(item) = -1)

let loadWordsFromFile filename input = 
    System.IO.Path.Combine(System.Environment.CurrentDirectory, filename) 
    |> System.IO.File.ReadAllLines
    |> Array.take 50
    |> Array.filter (fun i -> i.Length > 2)
    |> Array.filter ((fun i -> hasUnusedChars input i) >> not)
    |> Array.sortByDescending (fun i -> i.Length)
    |> Array.map stringItem
let loadWords input = loadWordsFromFile "wordlist.txt" input

let rec uberCandidatesRec (input : string) (lengthLeft : int) (runningSet : string[]) (action : string[] -> unit) (orderedPool : (string * int)[]) =

    orderedPool |> Array.Parallel.iteri (fun i item ->
        let itemValue, itemLength = item
        let itemLengthLeft = lengthLeft - itemLength
        let set = Array.append runningSet [|itemValue|]
        if itemLengthLeft > 0 && i < orderedPool.Length - 1 then 
            uberCandidatesRec input itemLengthLeft set action (Array.sub orderedPool (i + 1) (orderedPool.Length - i - 1)) 
        elif itemLengthLeft = 0 && alphabetize(String.Concat(set)) = alphabetize(input) then
            action set
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

let checkSentanceForHash (md5hash : string) (foundAction : string[] -> unit) (words : string[]) =
    let sentanceCandidates = System.Collections.Generic.List<string[]>()
    let all = getPermutations sentanceCandidates words []
    sentanceCandidates.ToArray() |> Array.iter (fun item -> 
        if checkHash item md5hash then foundAction item
        )    
        
    
let main () =
    let anagram = "poultry outwits ants" 
    let md5hash = "4624d200580677270a54ccff86b9610e" 

    let value = "poultryoutwitsants" //"poultryoutwitsants"
    let length = value.Length

    let words = loadWords value  // candidatesFromFile("ant")

    printfn "--------- List Candidates --------------"
    words |> uberCandidatesRec value length [||] (fun item ->
        System.IO.File.AppendAllText(System.IO.Path.Combine(System.Environment.CurrentDirectory, "result.txt") , item |> AsSentence)
        item |> printCandidate
        item |> checkSentanceForHash md5hash (fun bingo ->
            System.IO.File.AppendAllText(System.IO.Path.Combine(System.Environment.CurrentDirectory, "bingo.txt"), item |> AsSentence)
            printfn "BINGO!!!"            
            ) 
        ) 

    //words |> printWords
    // let expectedHash = calculateHash anagram
//main()

        

