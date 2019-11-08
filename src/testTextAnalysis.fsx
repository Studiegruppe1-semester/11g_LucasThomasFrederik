Open textAnalysis

printfn "White-box testing of readText"
printfn "readText: testTextAnalysis.fsx = testTextAnalysis.fsx : %A" (readText "testTextAnalysis.fsx")
// spørg ind til testen her

printfn "convertText 'kdjlh laiudf' = kdjlh laiudf : %A" (convertText "kdjlh laiudf")
printfn "convertText 'Kf.19.d,k2' = kfdk : %A" (convertText "Kf.19.d,k2")
printfn "convertText:      =       : %A" (convertText "    ")
printfn "convertText: 11D9 kD91D = d kdd : %A" (convertText "11D9 kD91D")
printfn "convertText: *dj, ud. 92 = dj ud  : %A" (convertText "*dj, ud. 92")
printfn "convertText: JD 13ID kd Jd = jd id kd jd : %A" (convertText "JD 13ID kd Jd")

printfn "histogram: JAmdamNDN = [2; 0; 0; 2; 0; 0; 0; 0; 0; 1; 0; 0; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] : %b" (histogram "JAmdamNDN" = [2; 0; 0; 2; 0; 0; 0; 0; 0; 1; 0; 0; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0])
printfn "histogram: oædk jasælkf aæl kwj klkjfas = [3; 0; 0; 1; 0; 2; 0; 0; 0; 3; 5; 3; 0; 0; 1; 0; 0; 0; 2; 0; 0; 0; 1; 0; 0; 0; 4] : %b" (histogram "oædk jasælkf aæl kwj klkjfas" = [3; 0; 0; 1; 0; 2; 0; 0; 0; 3; 5; 3; 0; 0; 1; 0; 0; 0; 2; 0; 0; 0; 1; 0; 0; 0; 4])
printfn "histogram: 391dj idj f89hfba = [1; 1; 0; 2; 0; 2; 0; 1; 1; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2] : %b" (histogram "391dj idj f89hfba" = [1; 1; 0; 2; 0; 2; 0; 1; 1; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2])
printfn "histogram: abcdefghijklmnopqrstuvwxyz = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0] : %b" (histogram "abcdefghijklmnopqrstuvwxyz" = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0])
printfn "histogram: icd* d#du/dk _ gdagcy = [1; 0; 2; 5; 0; 0; 2; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0; 3] : %b" (histogram "icd* d#du/dk _ gdagcy" = [1; 0; 2; 5; 0; 0; 2; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0; 3])
printfn "histogram: abcd abcd abcd = [3; 3; 3; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2] : %b" (histogram "abcd abcd abcd" = [3; 3; 3; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2])
// spørg ind til om det fylder for meget
// vil det være bedre med booleans

printfn "Testing 7g"
let TheStory = convertText (readText "littleClausAndBigClaus.txt")
let histTheStory = histogram TheStory
printfn "7g5 a"
printfn "Histogram of The Story : %A" histTheStory
printfn "7g5 b"
// printfn "7g5 b)"
// // printfn "Randomstring from the histogram of the story \n %A" randomText
let randomText = randomString histTheStory TheStory.Length
printfn "Histogram of randomText %A" (histogram randomText)
printfn "7g5 c"
printfn "Difference between the Histograms : %A" (diff histTheStory (histogram randomText))
printfn "7g5 d"
printfn "Testing the difference: (diff histTheStory (histogram randomText)) <= 500.0: %b" ((diff histTheStory (histogram randomText)) <= 500.0)

printfn "7g7: white box testing af cooccurence"
printfn "%A" ((cooccurrence "ababb").[0].[0..1])
printfn "cooccurrence: (cooccurence ''ababb'') [0].[0..1] = [0; 2] : %b" ((cooccurrence "ababb").[0].[0..1] = [0; 2])
printfn "%A" ((cooccurrence "ababb").[1].[0..1] = [1; 1])
printfn "cooccurrence: (cooccurrence ''ababb'').[1].[0..1] = [1; 1] : %b" ((cooccurrence "ababb").[1].[0..1] = [1; 1])
printfn "%A" ((cooccurrence "ababb").[4].[0..1] = [0; 0])
printfn "cooccurrence: (cooccurrence ''ababb'').[4].[0..1] = [0; 0] : %b" ((cooccurrence "ababb").[4].[0..1] = [0; 0])
printfn "%A" ((cooccurrence "ababb").[1].[0..4] = [0; 0])
printfn "cooccurrence: (cooccurrence ''ababb'').[4].[0..1] = [0; 0] : %b" ((cooccurrence "ababb").[4].[0..1] = [0; 0])

printfn "7g9"
printfn "7g9 a \n"
let cooc = cooccurrence TheStory
for i=0 to (List.length alphabet)-1 do
  for j=0 to (List.length alphabet)-1 do
      printf "%3d " cooc.[i].[j]
  printf "\n"
printfn "7g9 b \n"
let randomTextcooc = markovChain cooc TheStory.Length
let rCooc = cooccurrence randomTextcooc
for i=0 to (List.length alphabet)-1 do
  for j=0 to (List.length alphabet)-1 do
      printf "%3d " rCooc.[i].[j]
  printf "\n"
printfn "7g9 c"
printfn "Difference between the cooccurence histograms : %A" (diff2 cooc rCooc)
printfn "7g9 d"
printfn "Testing the difference: (diff2 cooc rCooc) <= 30.0 : %b" ((diff2 cooc rCooc) <= 27.0)