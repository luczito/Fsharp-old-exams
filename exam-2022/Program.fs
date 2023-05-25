open System
open Exam2022
open JParsec.TextParser

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    
    printfn "%A" (countWhite (Square 123uy)) // = 0
    printfn "%A" (countWhite img) // = 2

    printfn "%A" (rotateRight (Square 123uy)) // Square 123uy
    printfn "%A" (rotateRight (Quad(Square 0uy, Square 85uy, Square 170uy, Square 255uy))) //Quad (Square 255uy, Square 0uy, Square 85uy, Square 170uy)
    printfn "%A" (rotateRight img) // grayscale = Quad (Square 0uy, Square 255uy, Square 128uy, Quad (Square 64uy, Square 255uy, Square 128uy, Square 192uy))

    printfn "%A" (map (fun x -> Square (x + 10uy)) (Square 0uy)) // Square 10uy
    printfn "%A" (map (fun x -> Square (x + 10uy))(Quad (Square 0uy, Square 85uy, Square 170uy, Square 255uy)))
    (*
        Quad (Square 10uy, Square 95uy, Square 180uy, Square 9uy)
    *)
    printfn "%A" (map (fun x -> Quad (Square (x + 10uy), Square (x + 20uy), Square (x + 30uy), Square (x + 40uy))) (Square (123uy)))
    (*
        Quad (Square 133uy, Square 143uy, Square 153uy, Square 163uy)
    *)

    printfn "%A" (bitmap (Square 120uy)) //Square 0uy
    printfn "%A" (bitmap (Square 150uy)) // Square 255uy
    printfn "%A" (bitmap img ) //Quad (Square 255uy, Square 255uy, Quad (Square 255uy, Square 255uy, Square 255uy, Square 0uy), Square 0uy
    
    printfn "%A" (fold (fun acc x -> acc + int x) 0 (Square 123uy)) // 123
    printfn "%A" (fold (fun acc x -> acc + int x) 0 (Quad (Square 0uy, Square 85uy, Square 170uy, Square 255uy)))
    //510
    printfn "%A" (fold (fun acc x -> acc + int x) 0 img) //1022

    printfn "%A" (countWhite2 (Square 123uy)) // 0
    printfn "%A" (countWhite2 img) //2
    ()

let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    ()

let testQ3 () =
    printfn "Testing Question 3"
    

    // printfn "%A" (failDimensions (init (fun _ _ -> 0) 3 4) (init (fun _ _ -> 1) 8 9))
    //System.Exception: Invalid matrix dimensions: m1 rows = 3, m1 columns = 4, m2 roms = 8, m2 columns = )
    //q3.4
    printfn "%A" (add (init (fun x y -> x + y) 2 3) (init (fun x y -> x * y) 2 3) |> print)
    //0 1 2
    //1 3 5

    // printfn "%A" (add (init (fun x y -> x + y) 2 3) (init (fun x y -> x * y) 2 3) |> print)
    //System.Exception: Invalid matrix dimensions: m1 rows = 3, m1 columns = 4, m2 roms = 8, m2 columns = )

    //q3.3
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3)
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)
    printfn "%A" (dotProduct m1 m2 0 1) //28
    printfn "%A" (dotProduct m1 m2 1 0) //49
    
    ()

let testQ4 () =
    printfn "Testing Question 4"
    
    //q4.2
    printfn "%A" (runStackProgram [Push 5]) // 5
    printfn "%A" (runStackProgram [Push 5; Push 4; Add; Push 8; Mult]) // 72
    printfn "%A" (runStackProgram [Push 5; Push 4; Add; Push 8; Mult; Push 42; Add]) //114

    //q4.3
    printfn "%A" (push 5 >>>= push 6 >>>= pop |> evalSM) //Some(6, {5})
    printfn "%A" (pop |> evalSM) // None

    //q4.4
    printfn "%A" ( [Push 5] |> runStackProg2 |> evalSM |> Option.map fst) //Some 5
    printfn "%A" ( [Push 5; Push 4; Add; Push 8; Mult] |> runStackProg2 |> evalSM |> Option.map fst) //Some 72
    printfn "%A" ([Push 5; Push 4; Add; Push 8; Mult; Push 42; Add] |> runStackProg2 |> evalSM |> Option.map fst) //Some 114
    printfn "%A" ([Push 5; Push 4; Add; Push 8; Mult; Mult] |> runStackProg2 |> evalSM |> Option.map fst) //None
    printfn "%A" ([] |> runStackProg2 |> evalSM |> Option.map fst) //None
    
    //q4.5
    printfn "%A" ("PUSH 5\nPUSH 4 \nADD \n PUSH 8\nMULT \n" |>
                    parseStackProg |> getSuccess |> runStackProg2 |> evalSM |> Option.map fst)
                    //Some 72
    ()

[<EntryPoint>]
let main argv =
    testQ1 ()
    testQ2 ()
    testQ3 ()
    testQ4 ()
    0 // return an integer exit code
