open textAnalysis

printfn "7g4"
printfn "White-box testing of readText"
printfn "readText: readText 'testTextAnalysis.fsx' = testTextAnalysis.fsx : %A" (readText "testTextAnalysis.fsx")
printfn "readText: readText 'notAFile.fsx' = '' : %b" (readText "notAFile.fsx" = "")

printfn "White-box testing of convertText"
printfn "convertText: convertText 'kdjlh laiudf' = 'kdjlh laiudf' : %b" (convertText "kdjlh laiudf" = "kdjlh laiudf")
printfn "convertText: convertText 'Kf.19.d,k2' = 'kfdk' : %b" (convertText "Kf.19.d,k2" = "kfdk")
printfn "convertText: convertText '    ' = '    ' : %b" (convertText "    " = "    ")
printfn "convertText: convertText '11D9 kD91D' = 'd kdd' : %b" (convertText "11D9 kD91D" = "d kdd")
printfn "convertText: convertText '*dj, ud. 92' = 'dj ud '  : %b" (convertText "*dj, ud. 92" = "dj ud ")
printfn "convertText: convertText 'JD 13ID kd Jd' = 'jd id kd jd' : %b" (convertText "JD 13ID kd Jd" = "jd id kd jd")

printfn "White-box testing of histogram"
printfn "histogram: JAmdamNDN = [2; 0; 0; 2; 0; 0; 0; 0; 0; 1; 0; 0; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] : %b" (histogram "JAmdamNDN" = [2; 0; 0; 2; 0; 0; 0; 0; 0; 1; 0; 0; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0])
printfn "histogram: oædk jasælkf aæl kwj klkjfas = [3; 0; 0; 1; 0; 2; 0; 0; 0; 3; 5; 3; 0; 0; 1; 0; 0; 0; 2; 0; 0; 0; 1; 0; 0; 0; 4] : %b" (histogram "oædk jasælkf aæl kwj klkjfas" = [3; 0; 0; 1; 0; 2; 0; 0; 0; 3; 5; 3; 0; 0; 1; 0; 0; 0; 2; 0; 0; 0; 1; 0; 0; 0; 4])
printfn "histogram: 391dj idj f89hfba = [1; 1; 0; 2; 0; 2; 0; 1; 1; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2] : %b" (histogram "391dj idj f89hfba" = [1; 1; 0; 2; 0; 2; 0; 1; 1; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2])
printfn "histogram: abcdefghijklmnopqrstuvwxyz = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0] : %b" (histogram "abcdefghijklmnopqrstuvwxyz" = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0])
printfn "histogram: icd* d#du/dk _ gdagcy = [1; 0; 2; 5; 0; 0; 2; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0; 3] : %b" (histogram "icd* d#du/dk _ gdagcy" = [1; 0; 2; 5; 0; 0; 2; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0; 3])
printfn "histogram: abcd abcd abcd = [3; 3; 3; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2] : %b" (histogram "abcd abcd abcd" = [3; 3; 3; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2])

printfn "7g5 \n"
let TheStory = convertText (readText "littleClausAndBigClaus.txt")
let histTheStory = histogram TheStory
printfn "7g5 a"
printfn "Histogram of The Story : \n %A" histTheStory
// printfn "The story histogram:\n %A" (List.zip alphabet histTheStory)
// printfn "7g5 b"
let randomText = randomString histTheStory TheStory.Length
// printfn "Randomstring from the histogram of the story \n %A" randomText
printfn "Histogram of randomText : \n %A" (histogram randomText)
// printfn "Resulting histogram:\n %A" (List.zip alphabet (histogram randomText))
printfn "7g5 c"
printfn "Difference between the Histograms : %A" (diff histTheStory (histogram randomText))
printfn "7g5 d"
printfn "Testing the difference: (diff histTheStory (histogram randomText)) <= 500.0: %b" ((diff histTheStory (histogram randomText)) <= 500.0)

printfn "7g7: white-box testing of cooccurence \n"
printfn "cooccurrence: (cooccurrence 'ababb').[0].[0..1] = [0; 2] : %b" ((cooccurrence "ababb").[0].[0..1] = [0; 2]) 
printfn "cooccurrence: (cooccurrence 'ababb').[1].[0..1] = [1; 1] : %b" ((cooccurrence "ababb").[1].[0..1] = [1; 1])
printfn "cooccurrence: (cooccurrence 'ababb').[4].[0..1] = [0; 0] : %b" ((cooccurrence "ababb").[4].[0..1] = [0; 0])
printfn "cooccurrence: (cooccurrence 'ababb').[1].[0..4] = [1; 1; 0; 0; 0] : %b" ((cooccurrence "ababb").[1].[0..4] = [1; 1; 0; 0; 0])
printfn "cooccurrence: (cooccurrence 'eddedde').[4].[0..5] = [0; 0; 0; 2; 0; 0] : %b" ((cooccurrence "eddedde").[4].[0..5] = [0; 0; 0; 2; 0; 0])
printfn "cooccurrence: (cooccurrence 'eddedde').[3].[0..5] = [0; 0; 0; 2; 2; 0] : %b" ((cooccurrence "eddedde").[3].[0..5] = [0; 0; 0; 2; 2; 0])
printfn "cooccurrence: (cooccurrence 'eddedde').[15].[0..5] = [0; 0; 0; 0; 0; 0] : %b" ((cooccurrence "eddedde").[15].[0..5] = [0; 0; 0; 0; 0; 0])

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
printfn "Testing the difference: (diff2 cooc rCooc) <= 29.0 : %b" ((diff2 cooc rCooc) <= 25.0)

printfn "7g11: white-box testing of wordHistogram"
printfn "wordHistogram: (wordHistogram 'hej med dig min ven' = [('hej', 1); ('med', 1); ('dig', 1); ('min', 1); ('ven', 1)]) : %b" (wordHistogram "hej med dig min ven" = [("hej", 1); ("med", 1); ("dig", 1); ("min", 1); ("ven", 1)])
printfn "wordHistogram: (wordHistogram 'hej med dig hej med dig hej med dig' = [('hej', 3); ('med', 3); ('dig', 3)] : %b" (wordHistogram "hej med dig hej med dig hej med dig" = [("hej", 3); ("med", 3); ("dig", 3)])
printfn "wordHistogram: (wordHistogram 'wababababa u i wababababa' = [('wababababa', 2); ('u', 1); ('i', 1)]) : %b" (wordHistogram "wababababa u i wababababa" = [("wababababa", 2); ("u", 1); ("i", 1)])
printfn "wordHistogram: (wordHistogram 'abc ba ba a' = [('abc'), 1); ('ba', 2); ('a', 1)]) : %b" (wordHistogram "abc ba ba a" = [("abc", 1); ("ba", 2); ("a", 1)])
printfn "wordHistogram: (wordHistogram 'a abc ba ba' = [('a', 1); ('abc',1); ('ba',2)]) : %b" (wordHistogram "a abc ba ba" = [("a", 1); ("abc",1); ("ba",2)])
printfn "wordHistogram: (wordHistogram '' = [('', 0)]) : %b" (wordHistogram "" = [("", 0)])

// printfn "7g13 a"
let storyWHist = wordHistogram TheStory
let storyWLen = (List.sum (histFromWHist storyWHist))
// printfn "7g13 b"
let randomWText = randomWords storyWHist storyWLen
let rTextWHist = wordHistogram randomWText
printfn "7g13 c"
printfn "Difference between the word-histograms : %A" (diffw storyWHist rTextWHist)
printfn "7g13 d"
printfn "Testing the difference: (diffw storyWHist rTextWHist) <= 4.5 : %b" (diffw storyWHist rTextWHist <= 4.5)


printfn "7g15"
let cat = [("a", [("hat", 1); ("cat", 1)]); ("and", [("a", 1)]); ("cat", []); ("hat", [("and", 1)])]
printfn "cooccurenceOfWords: (cooccurenceOfWords 'a hat and a cat') = %A : %b" (cat) ((cooccurenceOfWords "a hat and a cat") = cat)
printfn "cooccurenceOfWords: (cooccurenceOfWords '') = [] : %b" ((cooccurenceOfWords "") = [])
printfn "cooccurenceOfWords: (cooccurenceOfWords ' ') = [] : %b" ((cooccurenceOfWords " ") = [])
printfn "cooccurenceOfWords: (cooccurenceOfWords 'hej') = [('hej', [])]: %b" ((cooccurenceOfWords "hej") = [("hej", [])])
printfn "cooccurenceOfWords: (cooccurenceOfWords 'hej med') = [('hej', [('med', 1)]); ('med', [])]: %b" ((cooccurenceOfWords "hej med") = [("hej", [("med", 1)]); ("med", [])])
printfn "cooccurenceOfWords: (cooccurenceOfWords 'hej hej' = : %b" ((cooccurenceOfWords "hej hej") = [("hej", [("hej", 1)])])

printfn "7g17"
// printfn "7g17 a"
let StoryWCooc = cooccurenceOfWords TheStory
// printfn "7g17 b"
let rTextWMChain = wordMarkovChain StoryWCooc storyWLen
let rTextwCooc = cooccurenceOfWords rTextWMChain
let rTextLen = (List.sum (histFromWHist (wordHistogram rTextWMChain)))
printfn "words in the story %A, words in random text %A" rTextLen storyWLen
printfn "7g17 c"
printfn "Difference between the woord cooccurrence-histograms : %A" (diffw2 StoryWCooc rTextwCooc)
printfn "7g17 d"
printfn "Testing the difference: (diffw2 storyWHist rTextwCooc) <= 0.005 : %b" (diffw2 StoryWCooc rTextwCooc <= 0.005)
