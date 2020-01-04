open preditorPrey

/// <summary>Prints input as string.</summary>
/// <param name="res">Input to be printed.</param>
/// <returns>Unit.</returns>
//let prn res = printf "%s" res

type EnvironmentTest() = class
    member self.MouseMultiplied() =
        let ticks = 2
        let mice = 1
        let env = new Environments(3, 1, mice, 0)        
        for _i=1 to ticks do
            env.RunTick()
        (env.CountMice() = 4)
    member self.OwlEatMice() =
        let ticks = 1 
        let env = new Environments(2, 5, 1, 3)
        for _i=1 to ticks do
            env.RunTick()
        (env.CountMice() = 0)
    member self.MiceMoves() =
        let ticks = 1
        let coords = (0,0)
        let env = new Environments(2, 5, 0, 0)
        let mouse = new Mouse(coords, 5, 0)
        let mutable fields = env.Fields
        (addAnimal coords mouse fields)
        env.Fields <- fields
        for _i=1 to ticks do
            env.RunTick()
        ((Array2D.get env.Fields 0 0) = None && env.CountMice() = 1)
    member self.MiceDoesntMoveWhenMultiplying() =
        let ticks = 2
        let coords = (0,0)
        let env = new Environments(2, 1, 0, 0)
        let mouse = new Mouse(coords, 1, 0)
        let mutable fields = env.Fields
        (addAnimal coords mouse fields)
        env.Fields <- fields
        for _i=1 to ticks do
            env.RunTick()
        match Array2D.get env.Fields 0 0 with 
                None -> false
                | Some(animal) -> (animal.StartTick = 0 && env.CountMice() = 4)
    member self.MiceCantMove() =
        let ticks = 1
        let coords = (0,0)
        let env = new Environments(2, 5, 4, 0)
        match Array2D.get env.Fields 0 0 with 
                None -> false
                | Some(animal) -> ((animal.getCoordToGoTo coords env.Fields (fun _ -> false) = None) && env.CountMice() = 4)
    member self.owlMoves() =
        let ticks = 1
        let coords = (0,0)
        let env = new Environments(2, 5, 0, 0)
        let owl = new Owl(coords, 0)
        let mutable fields = env.Fields
        (addAnimal coords owl fields)
        env.Fields <- fields
        for _i=1 to ticks do
            env.RunTick()
        ((Array2D.get env.Fields 0 0) = None)
    member self.noAnimals() =
        let ticks = 1 
        let env = new Environments(2, 5, 0, 0)
        for _i=1 to ticks do
            env.RunTick()
        (env.CountMice() = 0)
    member self.runTests() =
        printfn "MouseMultiplied: %A" <| self.MouseMultiplied()
        printfn "OwlEatMice: %A" <| self.OwlEatMice()
        printfn "MiceMoves: %A" <| self.MiceMoves()
        printfn "MiceDoesntMoveWhenMultiplying: %A" <| self.MiceDoesntMoveWhenMultiplying()
        printfn "MiceCantMove: %A" <| self.MiceCantMove()
        printfn "owlMoves: %A" <| self.owlMoves()
        printfn "noAnimals: %A" <| self.noAnimals()
end

printfn "\n Environment White-box test \n"
let envTest = new EnvironmentTest()
envTest.runTests()

