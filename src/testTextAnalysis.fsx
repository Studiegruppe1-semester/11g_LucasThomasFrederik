Open textAnalysis
printfn "White-box testing of readText" 
printfn "readText: testTextAnalysis.fsx = testTextAnalysis.fsx : %A" (readText "testTextAnalysis.fsx")


printfn "convertText: kdjlh laiudf = kdjlh laiudf : %A" (convertText "kdjlh laiudf")
printfn "convertText: Kf.19.d,k2 = kfdk : %A" (convertText "Kf.19.d,k2")
printfn "convertText:      =       : %A" (convertText "    ")
printfn "convertText: 11D9 kD91D = d kdd : %A" (convertText "11D9 kD91D")
printfn "convertText: *dj, ud. 92 = dj ud  : %A" (convertText "*dj, ud. 92")
printfn "convertText: JD 13ID kd Jd = jd id kd jd : %A" (convertText "JD 13ID kd Jd")

printfn "histogram: %A" (histogram "")
printfn "histogram: %A" (histogram "")
printfn "histogram: %A" (histogram "")
printfn "histogram: %A" (histogram "")
printfn "histogram: %A" (histogram "")
printfn "histogram: %A" (histogram "")


printfn "White-box testing of continuedFraction"
printfn "   unit: cfrac2float"
printfn "   branch 1.a: cfrac2float [] = 0.0 : %5b" (cfrac2float [] = 0.0)
printfn "   branch 2.a: cfrac2float [2] = 2.0 : %5b" (cfrac2float [2] = 2.0)
printfn "   branch 2.b: cfrac2float [0] = 0.0 : %5b" (cfrac2float [0] = 0.0)
printfn "   branch 3.a: cfrac2float [3; 4; 12; 4] = 3.245 : %5b" (cfrac2float [3; 4; 12; 4] = 3.245)
printfn "   branch 3.b: cfrac2float [6; 8; 24; 8] = 6.12435567 : %5b" (cfrac2float [6; 8; 24; 8] - 6.12435567 <= 0.000000001)
printfn "   branch 3.c: cfrac2float [9; 4; 0; 4; 8] = 9.123076923 : %5b" (cfrac2float [9; 4; 0; 4; 8] - 9.123076923 <= 0.000000001)
printfn "   branch 3.c: cfrac2float [-3; 6; 7; 5] = -2.837104072 : %5b \n" (cfrac2float [-3; 6; 7; 5] - (-2.837104072) <= 0.000000001)
