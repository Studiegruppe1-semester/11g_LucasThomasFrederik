Open textAnalysis

printfn "White-box testing of readText" 
printfn "readText: testTextAnalysis.fsx = testTextAnalysis.fsx : %A" (readText "testTextAnalysis.fsx")
// spørg ind til testen her

printfn "convertText: kdjlh laiudf = kdjlh laiudf : %A" (convertText "kdjlh laiudf")
printfn "convertText: Kf.19.d,k2 = kfdk : %A" (convertText "Kf.19.d,k2")
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

// printfn "Testing 7g"
// let TheStory = readText "littleClausAndBigClaus.txt" 
// //The histogram function automatically converts The Story
// let histTheStory = histogram TheStory
// printfn "7g5 a)"
// printfn "Histogram of The Story : %A" histTheStory
// printfn "7g5 b)"
// let randomText = randomString histTheStory (convertText TheStory).Length
// // printfn "Randomstring from the histogram of the story \n %A" randomText
// printfn "Histogram of randomText %A" (histogram randomText)
// printfn "Difference between the Histograms : %A" (diff histTheStory (histogram randomText)) 
// printfn "Testing the difference: (diff histTheStory (histogram randomText)) <= 0.7 : %b" ((diff histTheStory (histogram randomText)) <= 0.7)

// let newtext = "hej mit navn er ib blaasdasdasdoaspjdaspod asdasdas oksjpjgmplasjdpoas jdaspodj"
// let histNewText = histogram newtext
// let randomText2 = histogram (randomString histNewText (newtext.Length))
// printfn "Histogram of newtext %A" (histNewText)
// printfn "Histogram of randomText2 %A" (randomText2)
// printfn "Difference between the Histograms : %A" (diff histNewText randomText2) 


// let TheStory = readText "littleClausAndBigClaus.txt" 
// let histTheStory = histogram TheStory
// let randomText = randomString histTheStory (convertText TheStory).Length
// printfn "Histogram of randomText %A" (histogram randomText)
// printfn "Difference between the Histograms : %A" (diff histTheStory (histogram randomText))
// printfn "Testing the difference: (diff histTheStory (histogram randomText)) <= 0.7 : %b" ((diff histTheStory (histogram randomText)) <= 0.7)

// let cooc = cooccurrence (convertText (readText ("littleClausAndBigClaus.txt")))
// for i=0 to (List.length alphabet)-1 do
//   for j=0 to (List.length alphabet)-1 do
//       printf "%3d " cooc.[i].[j]
//   printf "\n"