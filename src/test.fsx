let alphabet = ['a'..'z']@[' ']
let convertText (src:string) : string =
  String.collect (fun c -> if List.exists((=)c) alphabet then string c else "")
    (src.ToLower())
let histogram (str : string) : int list =
  List.init (alphabet.Length) (fun i -> 
    String.length (String.collect (fun c -> 
        if c = alphabet.[i] then string c else "") 
            (convertText str)))

let diff (h1: int list) (h2 : int list) : double =
    (List.fold 0 )/(List.length h1)

// kan laves rekursivt