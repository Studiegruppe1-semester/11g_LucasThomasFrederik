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
printfn ""
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

printfn "7g7: white-box testing of cooccurence" 
printfn ""
printfn "cooccurrence: (cooccurrence ''ababb'').[0].[0..1] = [0; 2] : %b" ((cooccurrence "ababb").[0].[0..1] = [0; 2]) 
printfn "cooccurrence: (cooccurrence ''ababb'').[1].[0..1] = [1; 1] : %b" ((cooccurrence "ababb").[1].[0..1] = [1; 1])
printfn "cooccurrence: (cooccurrence ''ababb'').[4].[0..1] = [0; 0] : %b" ((cooccurrence "ababb").[4].[0..1] = [0; 0])
printfn "cooccurrence: (cooccurrence ''ababb'').[1].[0..4] = [1; 1; 0; 0; 0] : %b" ((cooccurrence "ababb").[1].[0..4] = [1; 1; 0; 0; 0])
printfn "cooccurrence: (cooccurrence ''eddedde'').[4].[0..5] = [0; 0; 0; 2; 0; 0] : %b" ((cooccurrence "eddedde").[4].[0..5] = [0; 0; 0; 2; 0; 0])
printfn "cooccurrence: (cooccurrence ''eddedde'').[3].[0..5] = [0; 0; 0; 2; 2; 0] : %b" ((cooccurrence "eddedde").[3].[0..5] = [0; 0; 0; 2; 2; 0])
printfn "cooccurrence: (cooccurrence ''eddedde'').[15].[0..5] = [0; 0; 0; 0; 0; 0] : %b" ((cooccurrence "eddedde").[15].[0..5] = [0; 0; 0; 0; 0; 0])

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

printfn "7g11: white-box testing of wordHistogram"
printfn "wordHistogram: (wordHistogram 'hej med dig min ven' = [('hej', 1); ('med', 1); ('dig', 1); ('min', 1); ('ven', 1)]) : %b" (wordHistogram "hej med dig min ven" = [("hej", 1); ("med", 1); ("dig", 1); ("min", 1); ("ven", 1)])
printfn "wordHistogram: (wordHistogram 'hej med dig hej med dig hej med dig' = [('hej', 3); ('med', 3); ('dig', 3)] : %b" (wordHistogram "hej med dig hej med dig hej med dig" = [("hej", 3); ("med", 3); ("dig", 3)])
printfn "wordHistogram: (wordHistogram 'a abc ba ba' = [('a', 1); ('abc',1); ('ba',2)]) : %b" (wordHistogram "a abc ba ba" = [("a", 1); ("abc",1); ("ba",2)])
printfn "wordHistogram: (wordHistogram '' = [('', 0)]) : %b" (wordHistogram "" = [("", 0)])

// // Opgave 7g13
// let storyWHist = wordHistogram TheStory
// // printfn "7g13 a"
// // for i = 0 to ((storyWHist.Length)/3) do
// //   if (i <> ((storyWHist.Length)/3)) then
// //     printfn "%d: %A %d: %A %d: %A" (3*i+2) (storyWHist.[3*i+2]) (3*i+1) (storyWHist.[3*i+1]) (3*i) (storyWHist.[3*i])
// //   else
// //     printfn "%d: %A %d: %A" (3*i+1) (storyWHist.[3*i+1]) (3*i) (storyWHist.[3*i])
// let randomWText = randomWords storyWHist (List.sum (histFromWHist storyWHist))
// // printfn "The randomly generated story \n"
// // printfn "%A" randomWText
// let rTextWHist = wordHistogram randomWText
// printfn "%A : %A" (List.sum (histFromWHist storyWHist)) (List.sum (histFromWHist rTextWHist))
// // printfn "7g13 b)"
// // for i = 0 to ((rTextWHist.Length)/3) do
// //   if (i <> ((rTextWHist.Length)/3)) then
// //     printfn "%d: %A %d: %A %d: %A" (3*i+2) (rTextWHist.[3*i+2]) (3*i+1) (rTextWHist.[3*i+1]) (3*i) (rTextWHist.[3*i])
// //   else
// //     printfn "%d: %A %d: %A" (3*i+1) (rTextWHist.[3*i+1]) (3*i) (rTextWHist.[3*i])


printfn "7g14"
let cat = [("a", [("hat", 1); ("cat", 1)]); ("and", [("a", 1)]); ("cat", []); ("hat", [("and", 1)])]
printfn "cooccurenceOfWords: (cooccurenceOfWords 'a hat and a cat' = %A : %b" (cat) ((cooccurenceOfWords "a hat and a cat") = cat)
// printfn "%A" (cooccurenceOfWords "a hat and a cat")
// printfn "%A" StoryWCooc

// [('a', [('hat', 1); ('cat', 1)]); ('and', [(”a”, 1)]); ('cat', []); ('hat', [('and', 1)])]
// [("a", [("hat", 1); ("cat", 1)]); ("and", [("a", 1)]); ("cat", []); ("hat", [("and", 1)])]