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

let rec getPermutations  (currentRunIndexes : list<int>) (permutationAction : string [] -> unit) (value : string[]) = 
    if currentRunIndexes.Length < value.Length  then
        for i in 0 .. value.Length - 1 do
            if not (currentRunIndexes |> List.exists(fun item -> item = i)) then 
                let currentRunIndexes = i :: currentRunIndexes 
                if value.Length = currentRunIndexes.Length then
                    let candidate = currentRunIndexes |> List.map (fun item -> value.[item]) |> List.toArray
                    permutationAction candidate
                else 
                    value |> getPermutations currentRunIndexes permutationAction
let possibleSentences (action : string [] -> unit) (value : string[]) =
    value |> getPermutations [] action 

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
    //|> Array.take 5000
    |> Array.filter (fun i -> i.Length > 4)
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
let ThreeSumCandidates (input : string) (action : string[] -> unit) (orderedPool : (string * int)[]) =
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
            printCandidate set
            if (aLength + bLength + cLength = 0) then
                if alphabetize(String.Concat(set)) = alphabetize(input) then 
                    action set
                start <- start + 1
                finnish <- finnish - 1
            elif (aLength + bLength + cLength > 0) then
                finnish <- finnish - 1
            else 
                start <- start + 1

let checkSentanceForHash (md5hash : string) (foundAction : string[] -> unit) (words : string[]) =
    words |> possibleSentences (fun item -> 
        if checkHash item md5hash then foundAction item
        )
let runWith3Sum () =         
    let anagram = "poultry outwits ants" 
    let md5hash = "4624d200580677270a54ccff86b9610e" 

    let value = "poultryoutwitsants" //"poultryoutwitsants"
    let length = value.Length

    let words = loadWords value 

    let mutable x = [||]
    printfn "--------- List Candidates --------------"
    words |> ThreeSumCandidates value (fun item ->
        item |> printCandidate
        item |> checkSentanceForHash md5hash (fun bingo -> 
            x <- bingo
            printf " <---- BINGO!!!") 
        ) 
    x |> printCandidate
let run () =         
    let anagram = "poultry outwits ants" 
    let md5hash = "4624d200580677270a54ccff86b9610e" 

    let value = "poultryoutwitsants" //"poultryoutwitsants"
    let length = value.Length

    let words = loadWords value 

    printfn "--------- List Candidates --------------"
    words |> uberCandidatesRec value length [||] (fun item ->
        System.IO.File.AppendAllText(System.IO.Path.Combine(System.Environment.CurrentDirectory, "result.txt") , (item |> AsSentence) + "\n")
        item |> printCandidate
        item |> checkSentanceForHash md5hash (fun bingo ->
            System.IO.File.AppendAllText(System.IO.Path.Combine(System.Environment.CurrentDirectory, "bingo.txt"), (item |> AsSentence) + "\n")
            printfn "BINGO!!!"            
            ) 
        ) 

let main () =
    let value = "poultryoutwitsants" //"poultryoutwitsants"
    let length = value.Length
    let words = loadWords value 
    words |> Array.length |> printfn "Dictionary Count: %d"  
    ""
//runWith3Sum()
            

