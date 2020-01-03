open preditorPrey
open System.IO

let editFile (textToAdd : string) =
    let filename = "simulation.txt"
    try
        File.WriteAllText(filename, textToAdd)
    with _ -> ()

[<EntryPoint>]
let simulate args =
    let n = int args.[0]
    let T = int args.[1]
    let p = int args.[2]
    let M = int args.[3]
    let O = int args.[4]
    let mutable text = string ""
    let env = new Environments(n, p, M, O)
    text <- text + "Initial state" + ("| Mice:" + (string (env.CountMice())) + "\n")
    printfn "initial state:"
    env.Tegn()
    printfn "___"
    for i=1 to T do
        (env.RunTick())
        text <- text + "Tick number:" + (string i) + ("| Mice:" + 
            (string (env.CountMice())) + "\n")
        env.Tegn()
    editFile text
    0