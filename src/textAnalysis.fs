module textAnalysis

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

/// <summary> Convert the contents of a file into a string.</summary>
/// <param name = "filename"> Name of the file to be read.</param>
/// <returns> A string containing the contents of the given file.</returns>
let readText (filename : string) : string = 
  try
    let reader = System.IO.File.OpenText filename
    reader.ReadToEnd ()
  with
    _ -> "" // The file cannot be read, so we return an empty string

/// <summary> Converts all letters in a string to lowercase,
/// and removes any characters not in the alphabet. (any not a..z or whitespace).</summary>
/// <param name = "src"> The string to be converted.</param>
/// <returns> A lowercase string with only whitespace as special signs. </returns>
let convertText (src:string) : string =
  // We test if any value in the given string, which we already made lowercase,
  // are in the given alphabet. If the given value isn't,
  // we replace it with an empty string ("").
  String.collect (fun c -> 
    if List.exists((=)c) alphabet then 
      string c 
    else "") (src.ToLower())

/// <summary> Generate a histogram of the characters 'a'..'z' and ' ' 
/// in a given string.</summary>
/// <param name = "str"> Any string consisting of the characters: 'a'..'z' and
/// ' ' in any order. </param>
/// <returns> A list of character counts, with the first element is the count of
/// 'a's in str, second the count of 'b's etc. </returns>
let histogram (str : string) : int list =
  List.init (alphabet.Length) (fun i -> 
    String.length (String.collect (fun c -> 
      if c = (alphabet.[i]) then string c else "") 
        (convertText str)))

/// <summary> Compare two histograms as the 
/// average sum of squared differences.</summary>
/// <param name = "h1"> A histogram of counts of characters.</param>
/// <param name = "h2"> Another histogram of counts of charecters.</param>
/// <returns> A double representing the difference 
/// between the histograms. </returns>
let diff (h1 : (int list)) (h2 : (int list)) : (double) =
  if (h1.Length <> alphabet.Length || h2.Length <> h1.Length) then
    -1.0
  else
    (List.fold2 (fun acc elem1 elem2 ->
      acc + pown ((float elem1)-(float elem2)) 2) 0.0 h1 h2)
        /(float alphabet.Length)

/// <summary> Find the index of a given item in the given list</summary>
/// <param name = "i"> The counter/index or "position" in the list.</param>
/// <param name = "item"> The item to find the index of in the list.</param>
/// <returns> The index of the item in the given list,
/// returns -1 if the given item is not found in the list.</returns>
let rec find (i : int) (item : _) (lst : (_ list)) : int =
  if (i >= lst.Length || i <= -1) then
    -1
  else if lst.[i] = item then
    i
  else
    find (i+1) item lst

/// <summary> Add 1 to the count of a given pair in a histogram.</summary>
/// <param name = "ch1"> Index in the alphabet
/// of the first character of the pair.</param>
/// <param name = "ch2"> Index in the alphabet 
/// of the second character of the pair.</param>
/// <param name = "lst"> A histogram of character pairs in a text.
/// Initially a list of lists, each with the length of
/// the alphabet, and containing only 0.</param>
/// <returns> A histogram of .</returns>
let newLst (ch1 : int) (ch2 : int) (lst : (int list list)) : (int list list) =
  (List.mapi (fun i x ->
    if (i = ch1) then
      (List.mapi (fun j y -> if j = ch2 then (y + 1) else y ) x)
        else lst.[i]) lst)

/// <summary> Count the occurences of each pair of characters.</summary>
/// <param name = "src"> A text or string to analyze.</param>
/// <param name = "lst"> A histogram of occurences of pairs in the text.
/// Initially a list of lists, each with the length of
/// the alphabet, and containing only 0.</param>
/// <returns> The histogram of occurences of each pair in the text. </returns>
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
  coFunc src (List.init (alphabet.Length) (fun i ->
    List.init (alphabet.Length) (fun j ->
      0)))

/// <summary> Generate a random string with the given length, and character
/// pairs distributed according to the given cooccurrence histogram. </summary>
/// <param name = "len"> The length of the string to generate.</param>
/// <param name = "cooc"> A cooccurrence histogram to 
/// generate character pairs from.</param>
/// <param name = "str"> A string containing at least 1 char, where the last 
/// char in the string will be used to generate the next char.</param>
/// <returns> A string with the length len of randomly generated character
/// pairs, with each char being "randomly" generated from the 
/// (histogram of the) char before it.</returns>
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
  // We start by generating a char from the "histogram" of pairs where " "
  // is the initial character, since it is the most likely to give us a general
  // overview (or "histogram") of the characters,
  // and thats why it is the best starting choice

/// <summary> Compare two histograms of pairs of characters as the 
/// average sum of squared differences.</summary>
/// <param name = "c1"> A histogram of occurrences of char pairs.</param>
/// <param name = "c2"> Another histogram of occurrences of char pairs.</param>
/// <returns> A double representing the difference 
/// between the histograms.</returns>
let diff2 (c1 : (int list list)) (c2 : (int list list)) : double =
  if (c1.Head.Length <> alphabet.Length || c2.Head.Length <> alphabet.Length) then
    -1.0
  else
    List.sum (List.init alphabet.Length (fun i -> diff c1.[i] c2.[i]))/(float alphabet.Length)

type wordHistogram = (string * int) list

/// <summary> Generate a histogram of words in a string.</summary>
/// <param name = "src">A string. </param>
/// <returns> A histogram of each word in the given string.</returns>
let wordHistogram (src : string ) : wordHistogram =
  if src = (string "") || src = (string " ") then
    [("", 0)]
  else if (string src.[src.Length-1]) = (string " ") then
    List.countBy (fun (x : string) -> 
      (x)) (Array.toList (src.[0..src.Length-2].Split ' '))
  else
    List.countBy (fun (x : string) -> (x)) (Array.toList (src.Split ' '))

/// <summary> Extract a string from a tuple of type string*_ 
/// (_ meaning any).</summary>
/// <param name = "str">A string.</param>
/// <param name = "n"> Anything.</param>
/// <returns> A string.</returns>
let getWord (str : string, n : _) : string =
  str

/// <summary> Extract an integer value from a tuple of type string*int.</summary>
/// <param name = "str"> A string.</param>
/// <param name = "n"> An integer.</param>
/// <returns> The integer n.</returns>
let getVal (str : string, n : int) : int =
  n

/// <summary> Get a list of Integers resembling the occurense of each word
/// in a wordhistogram.</summary>
/// <param name = "wHist"> A wordhistogram.</param>
/// <returns>A list of Integers resembling the occurense of each word
/// in a wordhistogram.</returns>
let histFromWHist (wHist : wordHistogram) : (int list) =
  List.init wHist.Length (fun i -> (getVal wHist.[i]))

/// <summary> Return a random word from a given wordhistogram.</summary>
/// <param name = "wHist"> A wordhistogram.</param>
/// <returns> A random word from the given wordhistogram</returns>
let randomWord (wHist : wordHistogram) : string =
  let cumHist = cumSum (histFromWHist wHist)
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  getWord wHist.[i]

/// <summary> Generate a random string from a wordhistogram
/// with the given amount of words.</summary>
/// <param name = "wHist"> A histogram of words.</param>
/// <param name = "nWords"> The amount of words to generate.</param>
/// <returns> A randomly generated string, generated from a wordhistogram,
/// with the given amount of words.</returns>
let randomWords (wHist : wordHistogram) (nWords : int) : string =
  String.init nWords (fun _ -> string (randomWord wHist) + string " ")

/// <summary> Compare two word-histograms as the 
/// average sum of squared differences.</summary>
/// <param name = "wL"> A histogram of words</param>
/// <param name = "wS"> A histogram of words </param>
/// <param name = "wLHist"> A list of integers</param>
/// <param name = "wSHist"> A list of integers</param>
/// <returns> </returns>
let diffwHelp (wL : wordHistogram) (wS : wordHistogram) (wLHist : int list) (wSHist : int list) : (double) =
  let wSWords = List.init wS.Length (fun i -> (getWord wS.[i]))
  let wNS = (List.map (fun (str,y) -> if (find 0 str wSWords) <> -1 then (getVal wS.[find 0 str wSWords]) else 0) wL)
  (List.fold2 (fun acc elem1 elem2 ->
    acc + pown ((float elem1)-(float elem2)) 2) 0.0 wLHist wNS)

/// <summary> Compare two word-histograms as the 
/// average sum of squared differences.</summary>
/// <param name = "w1"> A histogram of words</param>
/// <param name = "w2"> Another histogram of words</param>
/// <remarks> Expects that the shortest wordhistogram either contains
/// all or some of the words in the longest wordhistogram, but no words 
/// not in the longest wordhistogram.</remarks>
/// <returns> Returns the difference between the two as the 
/// average sum of squared differences.</returns>
let diffw (w1 : wordHistogram) (w2 : wordHistogram) : (double) =
  (*
    First we find out which histogram is the longest,
    so we can use functions such as List.fold
    which require the lists to have the same length.
    In our case, either the lists would have the same length
    or the original story would be the longest
    since the random text might not contain all letters
  *)
  if w1.Length > w2.Length then
    (diffwHelp w1 w2 (histFromWHist w1) (histFromWHist w2))/(float w1.Length)
  else
    (diffwHelp w2 w1 (histFromWHist w2) (histFromWHist w1))/(float w1.Length)

type wordCooccurrences = (string * wordHistogram) list

/// <summary> Extract the wordhistogram from a tuple of type
/// string*wordhistogram</summary>
/// <param name = "str"> A word in the wordhistogram.</param>
/// <param name = "wHist"> A wordhistogram.</param>
/// <returns> A wordhistogram.</returns>
let getWHist (str : string, wHist : wordHistogram) : wordHistogram =
  wHist

/// <summary> Convert the type int*option to int</summary>
/// <param name = "x"> A number of type int option.</param>
/// <returns> An integer.</returns>
let exists (x : int option) =
    match x with
    | Some(x) -> x
    | None -> -1

/// <summary> Add a new word or pair of words 
/// to a wordcooccurrence histogram.</summary>
/// <param name = "wCooc"> A list of wordcooccurences. </param>
/// <param name = "wLst"> A list of strings. </param>
/// <param name = "curWord"> The current word to add to the list.</param>
/// <param name = "last"> Whether the word is the last in the </param>
/// <param name = "n"> the index of the current word. </param>
/// <returns> A wordcooccurrence list.</returns>
let changeWList (wCooc: wordCooccurrences) (wLst : (string list)) (curWord : string) (last : bool) (n : int) : wordCooccurrences =
  let fIndex = List.tryFindIndex (fun (st,n) -> st = curWord) wCooc
  // If the word is the the last in the text, there is no 
  // next word to pair it with, otherwise find the next word
  let add = if last then [] else ([(wLst.[n+1],1)])
  // If the given word doesn't exists as the first word of a
  // pair of words yet, then add it to the end of the list
  // with the wordhistogram of the next word.
  if (fIndex <> None) then
    List.map (fun x -> if (getWord x = curWord) then (curWord , (List.append (getWHist wCooc.[exists fIndex]) add)) else x) wCooc
  else
    (List.append wCooc ([(curWord, add)]))

/// <summary> Create a histogram of occurrences of pairs 
/// of words in a string. </summary>
/// <param name = "n"> An integer resembling the the current index.</param>
/// <param name = "wLst"> The string converted to a list of strings/words.</param>
/// <param name = "wCooc"> A histogram of the occurrences of pairs of words.</param>
/// <returns>A histogram of the occurrences of pairs of words.</returns>
let rec coocWordsHelper (n : int) (wLst : (string list)) (wCooc : wordCooccurrences) : wordCooccurrences =
  let last = (n >= wLst.Length-1)
  if last then
    (changeWList wCooc wLst wLst.[n] last n)
  else
    coocWordsHelper (n+1) wLst (changeWList wCooc wLst wLst.[n] last n)

/// <summary> Create a histogram of occurrences of pairs 
/// of words in a string.</summary>
/// <param name = "src"> A string/text.</param>
/// <returns> A histogram of the occurrences of pairs of words.</returns>
let cooccurenceOfWords (src : string) : wordCooccurrences =
  let srcText = (Array.toList (src.Split ' '))
  let wCoocs = (coocWordsHelper 0 srcText [])
  let wcList = (List.map (fun c -> 
    ((getWord c),(List.countBy (fun s -> (getWord s)) (getWHist c)))) wCoocs)
  (List.sortBy (fun l -> 
    (getWord l)) (List.filter (fun lst -> (getWord lst) <> (string "")) wcList))

/// <summary> Generate a new word randomly from a 
/// given wordcooccurence histogram</summary>
/// <param name = "wCooc"> A word cooccurrence histogram.</param>
/// <returns> A new word randomly generated from a 
/// given wordcooccurence histogram</returns>
let wHistFromWCoocs (wCooc : wordCooccurrences) : wordHistogram =
  let firstWord = (randomWord (List.init wCooc.Length (fun i -> 
    (getWord wCooc.[i], (List.sumBy (getVal) (getWHist wCooc.[i]))))))
  let fWIndex = (List.findIndex (fun (s,l) -> s = firstWord) wCooc)
  getWHist wCooc.[fWIndex]

/// <summary> Generate a string/text with nWords amount of words
/// randomly generated from the word cooccurrence histogram.</summary>
/// <param name = "nWords"> The amount of words to generate.</param>
/// <param name = "nCount"> </param>
/// <param name = "wCooc"> A word cooccurrence histogram.</param>
/// <param name = "wHist"> A wordhistogram </param>
/// <returns> A text with nWords amount of words
/// randomly generated from the word cooccurrence histogram</returns>
let rec wMChainHelper (nWords : int ) (nCount:int) (wCooc : wordCooccurrences) (wHist : wordHistogram) : string =
  if (wHist = [("", 1)]) then
    wMChainHelper (nWords) (nCount) wCooc (wHistFromWCoocs wCooc)
  else
    let word = (randomWord wHist)
    let wordIndex = (List.findIndex (fun x -> (getWord x) = word) wCooc)
    if (nCount >= nWords-1) then
      word
    else
      word + (string " ") + (wMChainHelper nWords (nCount+1) wCooc (getWHist wCooc.[wordIndex]))

/// <summary> Generate a string/text with nWords amount of words
/// randomly generated from the word cooccurrence histogram.</summary>
/// <param name = "wCooc"> A word cooccurrence histogram.</param>
/// <param name = "nWords"> The amount of words to generate.</param>
/// <returns> A text with nWords amount of words
/// randomly generated from the word cooccurrence histogram.</returns>
let wordMarkovChain (wCooc : wordCooccurrences) (nWords : int) : string =
  (wMChainHelper nWords (0) wCooc [("",1)])

/// <summary> Make the shortest wordcooccurrence histogram
/// into the length of the longest.</summary>
/// <param name = "wS"> The shortest wordcooccurrence histogram</param>
/// <param name = "wL"> The longest wordcooccurrence histogram</param>
/// <returns> A new wordcooccurrence histogram</returns>
let newWordList (wS:wordCooccurrences) (wL:wordCooccurrences) : wordCooccurrences =
  let wSWords = List.init wS.Length (fun i -> (getWord wS.[i]))
  (List.map (fun c -> 
    if (find 0 (getWord c) wSWords) <> -1 then 
      (getWord c,getWHist wS.[find 0 (getWord c) wSWords]) 
    else
      (getWord c,List.init (getWHist c).Length (fun i -> 
        (getWord (getWHist c).[i]),0))) (wL))

/// <summary> Compare two word histograms by
/// finding the average sum of the squared differences. </summary>
/// <param name = "w1"> A word histogram</param>
/// <param name = "w2"> A word histogram</param>
/// <returns> A double resembling the difference between two wordhistograms.</returns>
let callDiffwHelper (w1 : wordHistogram) (w2 : wordHistogram) : (double) =
  if (histFromWHist w1).Length > (histFromWHist w2).Length then
    diffwHelp w1 w2 (histFromWHist w1) (histFromWHist w2)
  else
    diffwHelp w2 w1 (histFromWHist w2) (histFromWHist w1)

/// <summary> Compare two word-cooccurrence histograms by
/// finding the average sum of the squared differences. </summary>
/// <param name = "wL"> The longest wordcooccurence histogram.</param>
/// <param name = "wS"> The shortest wordcooccurence histogram.</param>
/// <returns> A double resembling the difference 
/// between two word-cooccurrence histograms.</returns>
let diffw2Help (wL : wordCooccurrences) (wS : wordCooccurrences) : (double) =
  let wNS = newWordList wS wL
  (List.fold2 (fun acc wCooc1 wCooc2 -> acc + (callDiffwHelper (getWHist wCooc1) (getWHist wCooc2))) 0.0 wNS wL)

/// <summary> Compare two word-cooccurrence histograms by
/// finding the average sum of the squared differences.</summary>
/// <param name = "c1"> A word-cooccurrence histogram of a string.</param>
/// <param name = "c2"> A word-cooccurrence histogram of another string.</param>
/// <remarks> Expects that the shortest histogram either contains
/// all or some of the words in the longest, but no words not in the
/// longest histogram. </remarks>
/// <returns> A double resembling the difference between two</returns>
let diffw2 (c1 : wordCooccurrences) (c2 : wordCooccurrences) : double =
  if c1.Length > c2.Length then
    (diffw2Help c1 c2)/(float (pown c1.Length 2))
  else
    (diffw2Help c2 c1)/(float (pown c2.Length 2))