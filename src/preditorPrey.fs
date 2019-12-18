module preditorPrey

let rnd = System.Random()

[<AbstractClass>]
type Animal(startCoordinate : (int*int), startTick : int) = class
    let mutable coords = startCoordinate
    let mutable lastMovedTick = startTick
    member self.Coordinate
        with get() = coords
        and set(k: (int*int)) = coords <- k
    member self.LastMovedTick
        with get() = lastMovedTick
        and set(k:int) = lastMovedTick <- k
    abstract member Move : (Animal option [,]) -> (int * int) option
    default self.Move(_) = None
end

let addAnimal (x:int, y:int) (anim: (Animal)) (fields : (Animal option) [,]) =
    Array2D.set fields x y (Some(anim))

let addCoords (a : (int*int)) (b : (int*int)) : (int*int) =
    (fst a + fst b, snd a + snd b)

let getCoordToGoTo (coords : (int*int)) (fields : (Animal option) [,]) (filter : (Animal -> bool)) : ((int*int) option) =
    let mutable lst: ((int*int) list) = List.empty
    if fst coords <> 0 then
        lst <- (addCoords (-1, 0) coords)::lst
    if fst coords <> ((Array2D.length1 fields)-1) then
        lst <- (addCoords (1, 0) coords)::lst
    if snd coords <> 0 then
        lst <- (addCoords (0, -1) coords)::lst
    if snd coords <> ((Array2D.length2 fields)-1) then
        lst <- (addCoords (0, 1) coords)::lst
    lst <- List.filter (fun coords -> 
        match Array2D.get fields (fst coords) (snd coords) with 
            None -> true
            | Some(thing) -> filter thing) lst
    if lst.IsEmpty then
        None
    else
        Some(lst.[rnd.Next(0,lst.Length-1)])

type Mouse(startCoordinate : (int*int), multiplyTicks : int, startTick : int) = class
    inherit Animal (startCoordinate, startTick)
    let mutable multiplyTimer = 0
    override self.Move(fs) =
        match getCoordToGoTo self.Coordinate fs (fun _ -> false) with
        None -> None
        | Some(coords) ->
            (if multiplyTimer >= multiplyTicks then
                addAnimal coords (Mouse(coords, multiplyTicks,self.LastMovedTick+1)) fs
                multiplyTimer <- 0
                None
            else
                multiplyTimer <- multiplyTimer+1
                Some(coords))
end

type Owl(startCoordinate : (int*int), startTick : int) = class
    inherit Animal (startCoordinate, startTick)
    override self.Move(fs) =
        getCoordToGoTo self.Coordinate fs (fun anim -> anim :? Mouse)
end

let makeAnewField (filter : (Animal -> bool)) (fields: (Animal option) [,]) (cur_tick : int) : (Animal option) [,] =
    fields |> Array2D.iteri (fun i j (anim : Animal option) ->
        Array2D.set fields i j (
            match anim with
                None -> None
                | Some(animal) ->
                    if animal.LastMovedTick >= cur_tick then Some(animal) else
                        if filter animal then Some(animal) else
                        (match animal.Move(fields) with
                            None -> Some(animal)
                            | Some((x,y)) -> animal.Coordinate <- (x,y); animal.LastMovedTick <- cur_tick; Array2D.set fields x y (Some(animal));None))
    )
    fields

let makeFields(size:int, multiplyTime : int, mice: int, owls: int, tick:int) =
    let mutable fields : ((Animal option) [,]) = Array2D.init size size (fun _ _ -> None)
    for _i=1 to mice do
        let mutable mCoord = (rnd.Next(size-1),rnd.Next(size-1))
        while (Array2D.get fields (fst mCoord) (snd mCoord)).IsSome do 
            mCoord <- (rnd.Next(size-1), rnd.Next(size-1))
        Array2D.set fields (fst mCoord) (snd mCoord) (Some (upcast Mouse(mCoord, multiplyTime,0)))
    for _j=1 to owls do
        let mutable owlCoord = (rnd.Next(size-1),rnd.Next(size-1))
        while (Array2D.get fields (fst owlCoord) (snd owlCoord)).IsSome do 
            owlCoord <- (rnd.Next(size-1), rnd.Next(size-1))
        Array2D.set fields (fst owlCoord) (snd owlCoord) (Some (upcast Owl(owlCoord,0)))
    fields

type Environments(size : int, multiplyTime : int, mice: int, owls : int) = class
    let mutable ticks = 0
    let mutable fields = makeFields (size, multiplyTime, mice, owls, ticks)

    member self.Fields
        with get() = fields
//        and set((x:int,y:int), animal : Animal option) = 
    member self.RunTick() =
        ticks <- ticks + 1
        let newFields = makeAnewField (fun a -> a :? Mouse) fields ticks
        fields <- makeAnewField (fun a -> a :? Owl) newFields ticks
    member self.Tegn() =
        for i=0 to size-1 do
            for j=0 to size-1 do
                let c = match (Array2D.get fields i j) with
                        None -> "+"
                        | Some(a) -> if a :? Mouse then "m" else if a :? Owl then "o" else "!"
                printf "%s" (c)
            printf "\n"
        printf "\n"
    member self.CountMice() =
        let mutable miceCount = 0
        Array2D.iter (fun (elem : Animal option) ->
            match elem with
                None -> ()
                | Some(elem) -> if (elem :? Mouse) then miceCount <- miceCount + 1
            ) fields
        miceCount
end