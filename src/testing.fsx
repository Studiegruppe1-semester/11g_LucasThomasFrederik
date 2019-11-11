/// <summary> Calculate the cumulative sum of a list of integers from the first
/// to the last element. First element is the first number in the original list,
/// last element is the sum of all integers in the original list. </summary>
/// <param name = "lst"> A list </param>
/// <returns> A cumulative summed list. E.g., for lst = [e1; e2; e3],
/// [e1; e1+e2; e1+e2+e3] is returned. </returns>
let cumSum (lst : int list) : int list =
  List.tail (List.scan (+) 0 lst)

/// <summary> Given a monotonic function and an index into its value set, find
/// the corresponding value on its definition set. </summary>
/// <param name = "monotonic"> A list of samples of a monotonically increasing
/// function. E.g., if monotonic = [e1; e2; e3] then e1 <= e2 <= e3 </param>
/// <param name = "v"> A value in the codomain of monotonic </param>
/// <returns> A value in the domain of monotonic approximately corresponding to
/// v. E.g., if monotonic.[i] = v then reverseLookup v = i </returns>
let reverseLookup (monotonic : 'a list) (v : 'a) : int =
    // `findIndex` will throw an exception if `v` is larger than all elements in
    // `monotonic`. The try-with expression will default to the with-clause if
    // such an exception is thrown.
    try
      List.findIndex (fun w -> w > v) monotonic
    with
      _ -> monotonic.Length - 1

// The random generator is created outside any function call. This is done to
// use a single seed for all random numbers, thus avoiding using the same seed
// more than once.
let rnd = System.Random()

// The remaining functions are specialized to work with the following alphabet
let alphabet = ['a'..'z']@[' ']

/// <summary> Generate a random character according to a histogram. </summary>
/// <param name = "hist"> A list of histogram values with count hist.[0] being
/// the value for 'a', hist.[1] for 'b' etc. </param>
/// <returns> A character randomly drawn from a distribution resembling
/// hist. </returns>
let randomChar (hist : int list) : char =
  let cumHist = cumSum hist
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  alphabet.[i] // Warning, this may cause an index out-of-bound exception

/// <summary> Generate a string of random characters each distributed according
/// to a histogram. </summary>
/// <param name = "hist"> A list of histogram values </param>
/// <param name = "len"> The length of the resulting string </param>
/// <returns> A string of length len whose values are independently drawn from a
/// distribution resembling hist </returns>
let randomString (hist : int list) (len : int) : string =
  String.init len (fun _ -> string (randomChar hist))

/// <summary> Given a string (a name of a file), tries to read the file.</summary>
/// <param name = "filename"> Name of a file.</param>
/// <returns> The contents of the given file as a string. </returns>
let readText (filename : string) : string = 
  try
    let reader = System.IO.File.OpenText filename
    reader.ReadToEnd ()
  with
    _ -> "" // The file cannot be read, so we return an empty string

/// <summary> Converts a string to lowercase letters and removes signs.</summary>
/// <param name = "src"> A string.</param>
/// <returns> A lowercase string with only whitespace as special signs. </returns>
let convertText (src:string) : string =
  // We test if any value in the given string, which we already made lowercase,
  // are in the given alphabet. If the given value isn't, 
  // we replace it with an empty string ("").
  String.collect (fun c -> if List.exists((=)c) alphabet then string c else "")
    (src.ToLower())
// Could have used match and checked for uppercase letters and replaced
// them with lowercase, else check if the character isn't in the alphabet.

/// <summary> Generate a histogram of the characters 'a'..'z' and ' ' in a given
/// string.</summary>
/// <param name = "str"> Any string consisting of the characters: 'a'..'z' and
/// ' ' in any order. </param>
/// <returns> A list of character counts, with the first element is the count of
/// 'a's in str, second the count of 'b's etc. </returns>
let histogram (str : string) : int list =
  List.init (alphabet.Length) (fun i -> 
    String.length (String.collect (fun c -> 
      if c = (alphabet.[i]) then string c else "") 
        (convertText str)))

/// <summary> </summary>
/// <param name = "h1"> </param>
/// <param name = "h2"> </param>
/// <returns>A double</returns>
let diff (h1 : (int list)) (h2 : (int list)) : (double) =
  if (h1.Length <> alphabet.Length || h2.Length <> alphabet.Length) then
    -1.0
  else
    (List.fold2 (fun acc elem1 elem2 ->
      acc + pown ((float elem1)-(float elem2)) 2) 0.0 h1 h2)
        /(float alphabet.Length)

/// <summary> Find the index of a given item in the given list</summary>
/// <param name = "i"> The counter/index or "position" in the list.</param>
/// <param name = "item"> The item to find the index of in the.</param>
/// <returns> The index of the item in the given list.
/// Returns -1 if the given item is not found in the list.</returns>
let rec find (i : int) (item : _) (lst : (_ list)) : int =
  // If we reached the last character in the alphabet, 
  // we assume this is the position
  if (i >= lst.Length || i <= -1) then
    -1
  else if lst.[i] = item then
    i
  else
    find (i+1) item lst

let newLst (ch1 : int) (ch2 : int) (lst : (int list list)) : (int list list) =
  (List.mapi (fun i x ->
    if (i = ch1) then
      (List.mapi (fun j y -> if j = ch2 then (y + 1) else y ) x)
        else lst.[i]) lst)

/// <summary> Counts the occurences of each pair of characters.</summary>
/// <param name = "src"> The text or string to analyze.</param>
/// <param name = "lst"> The list of the cooccurrences.</param>
/// <returns>A 2D array (array of arrays), listing the amount of times each 
/// pair is in the text. The array will be turned into a list</returns>
let rec coFunc (src : string) (lst : int list list) : (int list list) =
  // The last character in the text has no next character
  // to form a pair with.
  if (String.length src <> 1) then
    let ch1 = find 0 src.[0] alphabet
    let ch2 = find 0 src.[1] alphabet
    // We add 1 to the amount of times the given pair exists
    // at the given position.
    coFunc src.[1..] (newLst ch1 ch2 lst)
  else
    lst

/// <summary> Counts the occurences of each pair of characters.</summary>
/// <param name = "src"> The text or string to analyze.</param>
/// <returns> A list of lists, containing the amount of occurences each pair 
/// of characters appear in the text.</returns>
let cooccurrence (src : string) : (int list list) =
  // let arrLst =  coFunc src (Array2D.create alphabet.Length alphabet.Length 0)
  coFunc src (List.init (alphabet.Length) (fun i ->
    List.init (alphabet.Length) (fun j ->
      0)))

/// <summary> Generate a random string with the given length, and character
/// pairs distributed according to the given cooccurrence histogram. </summary>
/// <param name = "len"> The length of the string to generate.</param>
/// <param name = "cooc"> A cooccurrence histogram to generate char pairs from.</param>
/// <param name = "str"> A string containing at least 1 char, where the last 
/// char in the string will be used to generate the next char.</param>
/// <returns> A string with the length len of randomly generated character pairs,
/// with each char being "randomly" generated from the (histogram of the) char before it.</returns>
let rec markovChainHelper (len : int) (cooc : (int list list)) (str : string) : string =
  // This function is used to generate the random string.
  if (str.Length >= len-1) then
  // We call randomChar with the "histogram" of the current char
  // to generate the next char in the string.
    str + string (randomChar cooc.[find 0 str.[str.Length-1] alphabet])
  else
    markovChainHelper len cooc (str + string (randomChar cooc.[find 0 str.[str.Length-1] alphabet]))

/// <summary> Generate a random string with the given length, 
/// whose character pairs are distributed according to the given
/// cooccurrence histogram.</summary>
/// <param name = "cooc"> A cooccurrence histogram to generate char pairs from.</param>
/// <param name = "len"> The length of the string to generate.</param>
/// <returns> A randomly generated string with given length and character 
/// pairs distributed according to the cooccurrence histogram</returns>
let markovChain (cooc : (int list list)) (len : int) : string =
  // This function only generates the first random character in the
  // randomly generated string, the rest is created in markovCHainHelper
  markovChainHelper len cooc (string (randomChar cooc.[find 0 (char " ") alphabet]))
  // We start by generating a char from the "histogram" of pairs where " " (space)
  // is the initial character, since it is the most likely to give us a general
  // overview (or "histogram") of the characters,
  // and thats why it is the best starting choice

/// <summary> </summary>
/// <param name = "c1"> </param>
/// <param name = "c2"> </param>
/// <returns>A double</returns>
let diff2 (c1 : (int list list)) (c2 : (int list list)) : double =
  if (c1.Head.Length <> alphabet.Length || c2.Head.Length <> alphabet.Length) then
    -1.0
  else
    List.sum (List.init alphabet.Length (fun i -> diff c1.[i] c2.[i]))/(float alphabet.Length)

type wordHistogram = (string * int) list

/// <summary> </summary>
/// <param name = "src"> </param>
/// <returns> A histogram of each word in the given string.</returns>
let wordHistogram (src : string ) : wordHistogram =
  if src = (string "") || src = (string " ") then
    [("", 0)]
  else if (string src.[src.Length-1]) = (string " ") then
    List.countBy (fun (x : string) -> (x)) (Array.toList (src.[0..src.Length-2].Split ' '))
  else
    List.countBy (fun (x : string) -> (x)) (Array.toList (src.Split ' '))

let getWord (str : string, n : _) : string =
  str

let getVal (str : string, n : int) : int =
  n

let histFromWHist (wHist : wordHistogram) : (int list) =
  List.init wHist.Length (fun i -> (getVal wHist.[i]))

let randomWord (wHist : wordHistogram) : string =
  let cumHist = cumSum (histFromWHist wHist)
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  getWord wHist.[i]

/// <summary> </summary>
/// <param name = "src"> </param>
/// <returns> </returns>
let randomWords (wHist : wordHistogram) (nWords : int) : string =
  String.init nWords (fun _ -> string (randomWord wHist) + string " ")

/// <summary> </summary>
/// <param name = "src"> </param>
/// <returns> </returns>
let diffwHelp (wL : wordHistogram) (wS : wordHistogram) (wLHist : int list) (wSHist : int list) : (double) =
  let wSWords = List.init wS.Length (fun i -> (getWord wS.[i]))
  let wNS = (List.map (fun (str,y) -> if (find 0 str wSWords) <> -1 then (getVal wS.[find 0 str wSWords]) else 0) wL)
  (List.fold2 (fun acc elem1 elem2 ->
    acc + pown ((float elem1)-(float elem2)) 2) 0.0 wLHist wNS)/(float wL.Length)

/// <summary> </summary>
/// <param name = "src"> </param>
/// <returns> </returns>
let diffw (w1 : wordHistogram) (w2 : wordHistogram) : (double) =
  if (histFromWHist w1).Length > (histFromWHist w2).Length then
    diffwHelp w1 w2 (histFromWHist w1) (histFromWHist w2)
  else
    diffwHelp w2 w1 (histFromWHist w2) (histFromWHist w1)

type wordCooccurrences = (string * wordHistogram) list


let getWHist (str : string, wHist : wordHistogram) : wordHistogram =
  wHist


let exists (x : int option) =
    match x with
    | Some(x) -> x
    | None -> -1


let changeWList (wCooc: wordCooccurrences) (wLst : (string list)) (curWord : string) (last : bool) (n : int) : wordCooccurrences =
  let fIndex = List.tryFindIndex (fun (st,n) -> st = curWord) wCooc
  let add = if last then [] else ([(wLst.[n+1],1)])
  if (fIndex <> None) then
    List.map (fun x -> if (getWord x = curWord) then (curWord , (List.append (getWHist wCooc.[exists fIndex]) add)) else x) wCooc
  else
    (List.append wCooc ([(curWord, add)]))


let rec coocWordsHelper (n : int) (wLst : (string list)) (wCooc : wordCooccurrences) : wordCooccurrences =
  let last = (n >= wLst.Length-1)
  if last then
    (changeWList wCooc wLst wLst.[n] last n)
  else
    coocWordsHelper (n+1) wLst (changeWList wCooc wLst wLst.[n] last n)


let cooccurenceOfWords (src : string) : wordCooccurrences =
  let srcText = (Array.toList (src.Split ' '))
  let wCoocs = (coocWordsHelper 0 srcText [])
  let wcList = List.map (fun c -> ((getWord c),(List.countBy (fun s -> (getWord s)) (getWHist c)))) wCoocs
  List.sortBy (fun l -> (getWord l)) (List.filter (fun lst -> (getWord lst) <> (string "")) wcList)


let wHistFromWCoocs (wCooc : wordCooccurrences) (nWords : int) : wordHistogram =
  let firstWord = randomWord (List.init wCooc.Length (fun i -> (getWord wCooc.[i], (List.sumBy (getVal) (getWHist wCooc.[i])))))
  let fWIndex = (List.findIndex (fun (s,l) -> s = firstWord) wCooc)
  getWHist wCooc.[fWIndex]


let rec wMChainHelper (nWords : int ) (nCount:int) (wCooc : wordCooccurrences) (wHist : wordHistogram) : string =
  if (wHist = [("", 1)]) then
    wMChainHelper (nWords) (nCount) (wCooc) (wHistFromWCoocs wCooc (nWords-nCount))
  else
    let word = (randomWord wHist)
    let wordIndex = (List.findIndex (fun x -> (getWord x) = word) wCooc)
    if (nCount >= nWords-1) then
      word
    else
      word + (string " ") + (wMChainHelper nWords (nCount+1) wCooc (getWHist wCooc.[wordIndex]))

let wordMarkovChain (wCooc : wordCooccurrences) (nWords : int) : string =
  (wMChainHelper nWords (0) wCooc [("",1)])


let TheStory = (convertText (readText "littleClausAndBigClaus.txt"))
let histTheStory = histogram TheStory

// let storyWHist = wordHistogram TheStory
// let storyWLen = (List.sum (histFromWHist storyWHist))
// let StoryWCooc = cooccurenceOfWords TheStory

// let rTextWMChain = wordMarkovChain StoryWCooc storyWLen
// let rTextLen = (List.sum (histFromWHist (wordHistogram rTextWMChain)))
// printfn "%A" (rTextWMChain)
// printfn "%A : %A" rTextLen storyWLen
