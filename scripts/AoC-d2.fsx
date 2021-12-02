open System
open System.IO

let inputDataPath =
    let openDir = IO.Path.GetFullPath (__SOURCE_DIRECTORY__)
    Path.Combine(openDir,"..\supplements\d2-input.txt")

let inputData =
    File.ReadAllLines inputDataPath


// Part 1

type Coordinates = {
    X : int
    /// Depth! Not height!
    Y : int
} with
    member this.forward int = { this with X = this.X + int}
    /// INCREASE depth
    member this.down int = { this with Y = this.Y + int }
    /// DECREASE depth
    member this.up int = { this with Y = this.Y - int }

    /// Your horizontal position and depth both start at 0.
    static member init = {
        X = 0
        Y = 0
    }

    member this.distanceToZero = this.X * this.Y

let driveSubmarine =
    let mutable coordinates = Coordinates.init
    let downCommandStr = "down "
    let upCommandStr = "up "
    let forwardCommandStr = "forward "
    inputData
    |> Array.iter (fun commandStr ->
        match commandStr with
        | f when f.StartsWith(forwardCommandStr) -> 
            let units = f.Replace(forwardCommandStr,"") |> int
            coordinates <- coordinates.forward units
        | u when u.StartsWith(upCommandStr) -> 
            let units = u.Replace(upCommandStr,"") |> int
            coordinates <- coordinates.up units
        | d when d.StartsWith(downCommandStr) -> 
            let units = d.Replace(downCommandStr,"") |> int
            coordinates <- coordinates.down units
        | anythingElse ->
            failwith $"Unknown command {anythingElse}"
    )
    coordinates

/// Answer: 1990000
driveSubmarine.distanceToZero


// Part 2

type Traverse = {
    X : int
    /// Depth! Not height!
    Y : int
    Aim : int
} with
    /// It increases your horizontal position by X units.
    /// It increases your depth by your aim multiplied by X.
    /// exmp: forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40
    /// exmp: forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10, your depth increases by 2*10=20 to a total of 60.
    member this.forward int = 
        let increaseDepthBy = this.Aim * int
        { 
            this with 
                X = this.X + int 
                Y = this.Y + increaseDepthBy
        }
    /// increases your aim by X units.
    member this.down int = { this with Aim = this.Aim + int }
    /// decreases your aim by X units.
    member this.up int = { this with Aim = this.Aim - int }

    /// In addition to horizontal position and depth, you'll also need to track a third value, aim, which also starts at 0
    static member init = {
        X = 0
        Y = 0
        Aim = 0
    }

    member this.distanceToZero = this.X * this.Y

let driveSubmarine2 =
    let mutable coordinates = Traverse.init
    let downCommandStr = "down "
    let upCommandStr = "up "
    let forwardCommandStr = "forward "
    inputData
    |> Array.iter (fun commandStr ->
        match commandStr with
        | f when f.StartsWith(forwardCommandStr) -> 
            let units = f.Replace(forwardCommandStr,"") |> int
            coordinates <- coordinates.forward units
        | u when u.StartsWith(upCommandStr) -> 
            let units = u.Replace(upCommandStr,"") |> int
            coordinates <- coordinates.up units
        | d when d.StartsWith(downCommandStr) -> 
            let units = d.Replace(downCommandStr,"") |> int
            coordinates <- coordinates.down units
        | anythingElse ->
            failwith $"Unknown command {anythingElse}"
    )
    coordinates

/// Answer: 1990000
driveSubmarine2.distanceToZero