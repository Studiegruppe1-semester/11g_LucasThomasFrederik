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
    0.0
  else
    // let len = (List.fold (fun acc1 elem -> acc1 + (float elem)) 0.0 h1)
    ((List.fold2 (fun acc elem1 elem2 -> acc + pown ((float elem1)-(float elem2)) 2) 0.0 h1 h2)/float (List.rev (cumSum h1)).Head)

/// <summary> Finds the index of a given char in the alphabet</summary>
/// <param name = "i"> The counter/index or "position" in the alphabet.</param>
/// <param name = "ch"> The character to find the index of in the alphabet.</param>
/// <returns> The index of the character in the alphabet, 
/// or 26 if we have reached the last position in the alphabet</returns>
let rec find (i : int) (ch : char) : int =
  // If we reached the last character in the alphabet, 
  // we assume this is the position
  if i = (alphabet.Length-1) then
    alphabet.Length-1
  else if alphabet.[i] = ch then
    i
  else
    find (i+1) ch

/// <summary> Counts the occurences of each pair of characters.</summary>
/// <param name = "src"> The text or string to analyze.</param>
/// <param name = "arr"> The array in where the counts are remembered.</param>
/// <returns>A 2D array (array of arrays), listing the amount of times each 
/// pair is in the text. The array will be turned into a list</returns>
let rec coFunc (src : string) (arr : int [,]) : (int [,]) =
  // The last character in the text has no next character
  // to form a pair with.
  if (String.length src <> 1) then
    let ch1 = find 0 src.[0]
    let ch2 = find 0 src.[1]
    // We add 1 to the amount of times the given pair exists
    // at the given position.
    arr.[ch1,ch2] <- arr.[ch1,ch2]+1
    coFunc src.[1..] arr
  else
    arr

/// <summary> Creates a 2D array, and calls coFunc, which
/// counts the occurences of each pair of characters. The array is
/// then transformed into a list of lists.</summary>
/// <param name = "src"> The text or string to analyze.</param>
/// <returns> A list of lists, containing the amount of occurences each pair 
/// of characters appear in the text.</returns>
let cooccurrence (src : string) : (int list list) =
  let arrLst =  coFunc src (Array2D.create (alphabet.Length) (alphabet.Length) 0)
  List.init (alphabet.Length) (fun i ->
    List.init (alphabet.Length) (fun j ->
      arrLst.[i,j]))

