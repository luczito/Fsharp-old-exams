//q1

//q1.1
type Sum<'a, 'b> =
| Left of 'a
| Right of 'b

let value1 : Sum<int list, bool option> = Left [1; 2; 3]
let value2 : Sum<int list, bool option> = Right (Some true)

let sumMap (f: 'a -> 'c) (g: 'b -> 'c) (s: Sum<'a, 'b>) : 'c =
    match s with
    | Left x -> f x
    | Right y -> g y

sumMap List.length String.length (Left [1; 2; 3]) // = 3
sumMap List.length String.length (Right "Hello World!") // = 12

//q1.2

type SumColl<'a, 'b> =
| Nil
| CLeft of 'a * SumColl<'a, 'b>
| CRight of 'b * SumColl<'a, 'b>

let value3 = CLeft ([true; false], CRight (42, CLeft ([], Nil)))

let rec ofList (lst: Sum<'a, 'b> list) : SumColl<'a, 'b> =
    match lst with
    | [] -> Nil
    | Left x :: tail -> CLeft (x, ofList tail)
    | Right y :: tail -> CRight (y, ofList tail)

ofList [Left "Hello"; Right [1; 2; 3]; Left " world!!!"] // = CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil)))

//q1.3
let reverse (coll: SumColl<'a, 'b>) : SumColl<'a, 'b> =
    let rec loop (coll: SumColl<'a, 'b>) (acc: SumColl<'a, 'b>) : SumColl<'a, 'b> =
        match coll with
        | Nil -> acc
        | CLeft (x, tail) -> loop tail (CLeft (x, acc))
        | CRight (y, tail) -> loop tail (CRight (y, acc))
    loop coll Nil

reverse (CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil)))) //= CLeft (" world!!!", CRight ([1; 2; 3], CLeft ("Hello", Nil)))

//q1.4
let ofList2 (lst: Sum<'a, 'b> list) : SumColl<'a, 'b> =
    List.foldBack(
        fun sumElem acc ->
            match sumElem with
                | Left x -> CLeft (x, acc)
                | Right y -> CRight (y, acc))
            lst
            Nil
    
//q1.5
let rec foldBackSumColl (f: 'a -> 'c -> 'c) (g: 'b -> 'c -> 'c) coll acc =
    match coll with
    | Nil -> acc
    | CLeft (a, rest) -> f a (foldBackSumColl f g rest acc)
    | CRight (b, rest) -> g b (foldBackSumColl f g rest acc)
let coll = CLeft ("Hello", (CRight ([1; 2; 3], (CRight ([42], Nil)))))


foldBackSumColl 
    (fun s acc   -> String.length s + acc) 
    (fun lst acc -> List.length lst - acc)
    coll
    0


//q2-----------------------------------------------------------------------------------------------------------------

//q2.1
let f s =
    let l = String.length s
    let rec aux =
        function
        | i when i = l -> []
        | i -> s.[i] :: aux (i + 1)

    aux 0

let g s = 
    s |> f |>
    List.filter System.Char.IsLetter |>
    List.map System.Char.ToLower |>
    fun lst -> lst = List.rev lst

//What are the types of functions f and g?
//string -> char list
//string -> bool

//What do functions f and g do? Focus on what they do rather than how they do it.
//f: takes a string and returns a list of chars in the string.
//g: takes a string and returns a bool according to if the string is a palindrome.

//What would be appropriate names for functions f and g?
//toCharList
//isPalindrome

//q2.2
//Create a function f2 that behaves the same as f but which uses list comprehension.
let f2 (s: string) = Seq.toList s

let result = f2 "hello"

//q2.3
let g2 s =
    (f >>
    List.filter System.Char.IsLetter >>
    List.map System.Char.ToLower >>
    fun lst -> lst = List.rev lst) s

let resultg2 = g2 "dad"

//q2.4
//Because the recursive call is not made in the tail position of the function. If it were the call should be made as the last call before returning the result.


let eval = f "hello"
(*
    If called with hello:
    first we define l = 5 since its the length of hello.
    then aux is defined an int argument
    first call is i = 0 which calls aux i+1 (1)
    second call is i=1 which calls aux i+1 (2)
    third call is i=2 which calls aux i+1 (3)
    fourth call is i=3 which calls aux i+1 (4)
    fifth call is i=4 which calls aux i+1 (5)
    final call is i=5 which is i=l which returns []
*)

let fTail s = 
    let l = String.length s
    let rec loop acc i =
        if i = l then
            acc
        else 
            let newAcc = acc @ [s.[i]]  //add the char at index i to acc
            loop newAcc (i+1)

    loop [] 0 //start with acc = [] and i = 0

let fTailResult = fTail "hello"

//q2.5
let gOpt (s: string) =
    let rec compareChars start end' =
        match start, end' with
        | _, _ when start >= end' -> true
        | _, _ when not (System.Char.IsLetterOrDigit(s.[start])) -> compareChars (start + 1) end'
        | _, _ when not (System.Char.IsLetterOrDigit(s.[end'])) -> compareChars start (end' - 1)
        | _ when System.Char.ToLower(s.[start]) <> System.Char.ToLower(s.[end']) -> false
        | _ ->
            compareChars (start + 1) (end' - 1)

    compareChars 0 (String.length s - 1)

let gOpt2 (s: string) =
    let rec compare start end' = 
        let startChar = System.Char.ToLower s[start]
        let endChar = System.Char.ToLower s[end']
        if start >= end' then
            true
        elif (System.Char.IsLetterOrDigit startChar) then
            compare (start + 1) end'
        elif (System.Char.IsLetterOrDigit endChar) then
            compare start (end' - 1)
        elif startChar = endChar then
            compare (start + 1) (end' - 1)
        else 
            false
    compare 0 (String.length s - 1)

let gOptResult1 = gOpt "Dromedaren Alpotto planerade mord!!!"
let gOptResult2 = gOpt2 "Dromedaren Alpotto planerade mord!!!"

//----------------------------------------------------------------------------------------------------------------
//q3


//q3.1
let calculateGoldenRatio n =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ -> loop (n-1) (1.0 + 1.0 / acc)
    loop n 1.0

calculateGoldenRatio 0  // = 1.0
calculateGoldenRatio 3  // = 1.666666667
calculateGoldenRatio 42 // = 1.618033989

//q3.2
let grSeq =
    let rec loop count list = 
        let fibo = calculateGoldenRatio count
        let updatedList = Seq.append list (Seq.singleton fibo)
        if count < 1000 then
            loop (count + 1) updatedList
        else
            updatedList
    loop 0 Seq.empty

let infi = Seq.item 100 grSeq

//q3.3
let goldenRectangleSeq x =
    Seq.map (fun gr -> x * (gr * x)) grSeq

open System
let goldenTriangleSeq x =
    Seq.map (fun gr -> x * (x * (Math.Sqrt (((float)gr*(float)gr) - ((float)1/(float)4)))) / (float)2) grSeq


//q3.4
let goldenRectangleTriangle a =
    let pairs = seq {
        for gr in grSeq do
        let rect = a * (gr * a)
        let tria = a * (a * (Math.Sqrt (((float)gr*(float)gr) - ((float)1/(float)4)))) / (float)2
        yield (rect, tria)
    }
    pairs


//q4-----------------------------------------------------------------------------------------------------------

//q4.1
type tile = { Shape: string; Color: string }

let mkTile col shap =
    let tile = {Shape = shap; Color = col}
    tile

let tileToString t =
    let output = t.Shape + " " + t.Color
    output
     
//q4.2
let validTiles ts t =
    let isValid t =
        List.forall (fun existingTile ->
            (existingTile.Shape = t.Shape && existingTile.Color <> t.Color)
            || (existingTile.Color = t.Color && existingTile.Shape <> t.Shape)
        ) ts

    isValid t
        
//q4.3
type coord = Coord of int * int
type board = Board of Map<coord, tile>
type direction = Left | Right | Up | Down

let moveCoord c d =
    match c, d with
    | Coord(x, y), Left -> Coord(x-1, y)
    | Coord(x,y), Right -> Coord(x+1, y)
    | Coord(x,y), Up -> Coord(x, y-1)
    | Coord(x,y), Down -> Coord(x,y+1)

let collectTiles b c d =
    let rec loop c d acc =
        let getNextTile c =
            match b with
            | Board(tileMap) -> Map.tryFind c tileMap

        match getNextTile c with
        | Some(tile) ->
            let nextCoord = moveCoord c d
            loop nextCoord d (tile :: acc)
        | None -> List.rev acc
    loop c d


//q4.4

let (>>=) option bindFn =
    match option with
    | Some(value) -> bindFn value
    | None -> None

let placeTiles ts b =
    let placeTile c t b  =
        match b with
        | Board(tileMap) ->
            if Map.containsKey c tileMap then
                None
            else
                Some(Board(Map.add c t tileMap))

    let placeAllTiles ts b  =
        List.foldBack (fun (coord, tile) acc ->
            acc >>= fun b -> placeTile coord tile b
        ) ts (Some b)

    placeAllTiles ts b

    
