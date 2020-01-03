open preditorPrey

/// <summary>Prints input as string.</summary>
/// <param name="res">Input to be printed.</param>
/// <returns>Unit.</returns>
//let prn res = printf "%s" res

type EnvironmentTest() = class
    member self.MouseMultiplied() =
        let ticks = 2
        let mice = 1
        let env = new Environments(2, 1, mice, 0)        
        for _i=1 to ticks do
            env.RunTick()
        (env.CountMice() = 4)
    
    member self.OwlEatMice() =
        let ticks = 1 
        let env = new Environments(2, 5, 1, 3)
        for _i=1 to ticks do
            env.RunTick()
        (env.CountMice() = 0)

    member self.runTests() =
        printfn "MouseMultiplied: %A" <| self.MouseMultiplied()
        printfn "OwlEatMice: %A" <| self.OwlEatMice()
end

printfn "\n Environment White-box test \n"
let envTest = new EnvironmentTest()
envTest.runTests()

