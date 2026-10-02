open System

type Color =
    | Red = 1
    | Green = 2
    | Blue = 3

[<EntryPoint>]
let main argv =
    let greenish = Color.Red ||| Color.Blue
    let mutable color = Color.Green

    let isGreen c =
        match c with
        | Color.Green -> true
        | _ -> false

    Console.WriteLine($"Color = {color}")
    Console.WriteLine($"Is color green? {isGreen color}")
    0
