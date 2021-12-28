module Utils

open System


let (>=<) (x, y) (maxX, maxY) = 0 <= x && x <= maxX - 1 && 0 <= y && y <= maxY-1
let getDimensions (a2d:int[,]) = (a2d.GetLength(0), a2d.GetLength(1))
let printA2D (a2d:int[,]) =
    for x in 0..a2d.GetLength(0)-1 do
        for y in 0..a2d.GetLength(1)-1 do
            Console.Write ((string a2d[x,y]).PadLeft(2, ' '))
            Console.Write " "
        Console.WriteLine ""
    Console.WriteLine ""

