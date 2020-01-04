module preditorPrey

let rnd = System.Random()

/// <summary> Add the tuple-coordinates.</summary>
/// <param name = "a"> The first coordinate.</param>
/// <param name = "b"> The second coordinate.</param> 
/// <returns> The result of adding the coordinates.</returns>
let addCoords (a : (int*int)) (b : (int*int)) : (int*int) =
    (fst a + fst b, snd a + snd b)

/// <summary> The animal class.</summary>
/// <param name = "startCoordinate"> The coordinates which 
/// the animal starts on.</param>
/// <param name = "startTick"> The tick which the animal was created on.</param>
[<AbstractClass>]
type Animal(startCoordinate : (int*int), startTick : int) = class
    let mutable coords = startCoordinate
    let mutable lastMovedTick = startTick
    member self.StartTick = startTick
    member self.Coordinate
        with get() = coords
        and set(k: (int*int)) = coords <- k
    member self.LastMovedTick
        with get() = lastMovedTick
        and set(t:int) = lastMovedTick <- t
    abstract member Move : (Animal option [,]) -> (int * int) option
    default self.Move(_) = None

    /// <summary> Find a suitable field to move to,
    /// given a filter.</summary>
    /// <param name = "coords"> The coordinates of the animal which
    /// wants to move.</param>
    /// <param name = "fields"> The fields in which to check for
    /// coords to go to.</param>
    /// <param name = "filter"> A filter to check for 
    /// suitable neighouring fields.</param>
    /// <returns> Either a field which can be moved to,
    /// or None if there is no neighbouring field
    /// which meet the given filter requirements.</returns>
    member self.getCoordToGoTo (coords : (int*int)) (fields : (Animal option) [,]) (filter : (Animal -> bool)) : ((int*int) option) =
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
            Some(lst.[rnd.Next(lst.Length)])
end

/// <summary> Adds an animal to the fields, at the given coordinates.</summary>
/// <param name = "x"> The x-coordinate of the animal to be added.</param>
/// <param name = "y"> The x-coordinate of the animal to be added.</param>
/// <param name = "anim"> The animal to add, either a mouse or an owl.</param>
/// <param name = "fields"> The fields in which the animal is added to.</param>
/// <returns> The changed fields.</returns>
let addAnimal (x:int, y:int) (anim: (Animal)) (fields : (Animal option) [,]) =
    Array2D.set fields x y (Some(anim))

/// <summary> A mouse class.</summary>
/// <param name = "startCoordinate"> The coordinate the mouse started on.</param>
/// <param name = "multiplyTicks"> The amount of ticks 
/// before a mouse will multiply.</param>
/// <param name = "startTick"> The tick which the mouse was created on.</param>
type Mouse(startCoordinate : (int*int), multiplyTicks : int, startTick : int) = class
    inherit Animal (startCoordinate, startTick)
    let mutable multiplyCounter = 1
    /// <summary> Searches for a field the mouse can move to.</summary>
    /// <param name = "fs"> The fields which the mouse moves on.</param>
    /// <returns> A field the mouse can move to, 
    /// or none if none is available.</returns>
    override self.Move(fs) =
        match self.getCoordToGoTo self.Coordinate fs (fun _ -> false) with
            None -> None
            | Some(coords) ->
                (if multiplyCounter >= multiplyTicks then
                    addAnimal coords (Mouse(coords, multiplyTicks,self.LastMovedTick)) fs
                    multiplyCounter <- 1
                    None
                else
                    multiplyCounter <- multiplyCounter+1
                    Some(coords))
end

/// <summary> The owl class.</summary>
/// <param name = "startCoordinate"> The startcoordinate of the owl.</param>
/// <param name = "startTick"> The tick the owl was created on.</param>
type Owl(startCoordinate : (int*int), startTick : int) = class
    inherit Animal (startCoordinate, startTick)
    /// <summary> Searches for a field the owl can move to.</summary>
    /// <param name = "fs"> The fields which the owl moves on.</param>
    /// <returns> A field the owl can move to, 
    /// or none if none is available.</returns>
    override self.Move(fs) =
        self.getCoordToGoTo self.Coordinate fs (fun anim -> anim :? Mouse)
end

/// <summary> Creates the field which the animals move on,
/// and randomly distributes owls and mice across it </summary>
/// <param name = "size"> The size of the environment/fields to create.</param>
/// <param name = "multiplyTime"> The amount of ticks between</param>
/// <param name = "mice"> The amount of mice to create.</param>
/// <param name = "owls"> The amount of owls to create.</param>
/// <param name = "tick"> The current tick.</param>
/// <returns> An environment/Fields with randomly 
/// distributed mice and owls.</returns>
let makeFields(size:int, multiplyTime : int, mice: int, owls: int, tick:int) =
    let mutable fields : ((Animal option) [,]) = Array2D.init size size (fun _ _ -> None)
    for _i=1 to mice do
        let mutable mCoord = (rnd.Next(size),rnd.Next(size))
        while (Array2D.get fields (fst mCoord) (snd mCoord)).IsSome do 
            mCoord <- (rnd.Next(size), rnd.Next(size))
        Array2D.set fields (fst mCoord) (snd mCoord) (Some (upcast Mouse(mCoord, multiplyTime,0)))
    for _j=1 to owls do
        let mutable owlCoord = (rnd.Next(size),rnd.Next(size))
        while (Array2D.get fields (fst owlCoord) (snd owlCoord)).IsSome do 
            owlCoord <- (rnd.Next(size), rnd.Next(size))
        Array2D.set fields (fst owlCoord) (snd owlCoord) (Some (upcast Owl(owlCoord,0)))
    fields

/// <summary> The environment class.</summary>
/// <param name = "size"> The size of the environment/fields.</param>
/// <param name = "multiplyTime"> The amount of ticks before a
/// mouse multiplies.</param>
/// <param name = "mice"> The amount of mice at the beginning.</param>
/// <param name = "owls"> The amount of owls.</param>
type Environments(size : int, multiplyTime : int, mice: int, owls : int) = class
    let mutable ticks = 0
    let mutable fields = makeFields (size, multiplyTime, mice, owls, ticks)

    /// <summary> Runs through each field in the environment/fields,
    /// moving the owls and mice.</summary>
    /// <param name = "filter"> A filter to check for 
    /// suitable neighouring cordinates.</param>
    /// <param name = "fields"> The environment, the fields
    /// the mice and owls are located on.</param>
    /// <param name = "cur_tick"> The current tick.</param>
    /// <returns> The changed environment/fields.</returns>
    member self.changeFields (filter : (Animal -> bool)) (fields: (Animal option) [,]) (cur_tick : int) : (Animal option) [,] =
        fields |> Array2D.iteri (fun i j (anim : Animal option) ->
            Array2D.set fields i j (
                match anim with
                    None -> None
                    | Some(animal) ->
                        if animal.LastMovedTick >= cur_tick then Some(animal) else
                            if filter animal then Some(animal) else
                            animal.LastMovedTick <- cur_tick
                            (match animal.Move(fields) with
                                None -> Some(animal)
                                | Some((x,y)) -> animal.Coordinate <- (x,y); Array2D.set fields x y (Some(animal));None))
        )
        fields

    member self.Fields
        with get() = fields
        and set(nFields : (Animal option [,])) = fields <- nFields

    /// <summary> Runs the simulation for the fields/environment for 1 tick.</summary>
    /// <returns> The changed environment/fields.</returns>
    member self.RunTick() =
        ticks <- ticks + 1
        let newFields = self.changeFields (fun a -> a :? Mouse) fields ticks
        fields <- self.changeFields (fun a -> a :? Owl) newFields ticks

    /// <summary> Counts the mice</summary>
    /// <returns> The amount of mice in the fields/environment</returns>
    member self.CountMice() =
        let mutable miceCount = 0
        Array2D.iter (fun (elem : Animal option) ->
            match elem with
                None -> ()
                | Some(elem) -> if (elem :? Mouse) then miceCount <- miceCount + 1
            ) fields
        miceCount
end