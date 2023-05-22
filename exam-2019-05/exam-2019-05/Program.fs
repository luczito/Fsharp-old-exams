type Peano =
    | O
    | S of Peano


//q1 make toInt and fromInt functions:

//toInt matches peano and converts recursively
let rec toInt (a: Peano) : uint32 =
    match a with
    | O -> 0u //O is equal to 0
    | S p -> 1u + toInt p //else call recursively on p and add 1 until base case 0 is hit.


//fromInt matches int and casts to peano.
let rec fromInt (x: uint32) : Peano =
    match x with
    | 0u -> O //base case 0 = O
    | _ -> S (fromInt (x - 1u)) //else call recursively after subtrackting 1.


//q2 make add, mult and pow functions:
let rec add (a: Peano) (b: Peano) : Peano =
    match a with
    | O -> b  // If a is zero (O), return b as the sum of zero and b is b.
    | S p -> S (add p b)  // If a is S p (successor of p), recursively call add on p and b, then construct the successor of the pi.

let rec mult (a: Peano) (b: Peano) : Peano =
    match a with
    | O -> O  // If a is zero (O), return O as the product of zero and any number is zero.
    | S p -> add b (mult p b)  // If a is S p (successor of p), recursively call mult on p and b, then add b to the product of p and b.

let rec pow (a: Peano) (b: Peano) : Peano =
    match b with
    | O -> S O  // If b is zero (O), return S O as any number raised to the power of zero is 1.
    | S p -> mult a (pow a p)  // If b is S p (successor of p), recursively call pow on a and p, then multiply a with the pi.

add  (S (S O)) (S (S (S O))) // S (S (S (S (S O))))
mult (S (S O)) (S (S (S O))) // S (S (S (S (S (S O)))))
pow  (S (S O)) (S (S (S O))) // S (S (S (S (S (S (S (S O))))))) 


let tailAdd a b = 
    let rec aux a b acc =
        match a with 
            | O -> acc
            | S p -> aux p b (S (acc))
    aux a b b
let tailaddpi = tailAdd (S (S O)) (S (S (S O)))

let tailMult a b = 
    let rec aux a b acc =
        match a with
            | O -> acc
            | S p -> aux p b (tailAdd b acc)
    aux a b O

let tailMultpi: Peano = tailMult (S (S O)) (S (S (S O))) // S (S (S (S (S (S O)))))

let tailPow a b =
    let rec aux a b acc = 
        match b with 
            | O -> acc
            | S p -> aux a p (tailMult a acc)
    aux a b (S O)

let tailPowpi = tailPow  (S (S O)) (S (S (S O))) // S (S (S (S (S (S (S (S O))))))) 

let rec loop f acc p =
    match p with
        | O -> acc
        | S p -> loop f (f acc) p

loop not true O //true
loop not true (S (S (S O))) //false
loop not true (S (S (S (S (S (S (S (S O)))))))) // true


let loopAdd a b =
  let addOne x =
    match x with
    | O -> S O
    | S p -> S (S p)
  loop (fun acc -> addOne acc) a b

let loopAddpi = loopAdd (S (S O)) (S (S (S O)))

let loopMult a b =
    let addA x =
        loopAdd x a
    loop(fun acc -> addA acc) O b

let loopMultpi: Peano = loopMult (S (S O)) (S (S (S O))) // S (S (S (S (S (S O)))))

let loopPow a b = 
    let multA x = 
        loopMult x a
    loop(fun acc -> multA acc) (S O) b

let loopPowpi = loopPow  (S (S O)) (S (S (S O))) // S (S (S (S (S (S (S (S O))))))) 


//-------------------------------------------------------------------------------------------------------------
//OPG 2

//q 2.1

//f is of type "'a -> 'a list -> 'a list option"
//g is of type "'a list -> 'a list -> bool"

//f: it checks if x is in list y and if it is returns a list without x
//g: it checks if y is a permutation of list x.

//f: removeElementFromList
//g: isPermutation

//q 2.2
let rec f x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys when x <> y -> 
        match f x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None
//the error occurs because the function does not properly handle if the pi is none

let rec f2 x =
    function
    | [] -> None
    | y::ys when x = y -> Some ys
    | y::ys ->
        match f2 x ys with
        | Some ys -> Some(y::ys)
        | None -> None


//q2.3

let rec fOpt x = function
  | [] -> None
  | y::ys when x = y -> Some ys
  | y::ys -> fOpt x ys |> Option.map (fun ys' -> y::ys')

let rec gOpt xs = function
  | [] -> xs = []
  | y::ys -> fOpt y xs |> Option.map (fun xs' -> gOpt xs' ys) |> Option.defaultValue false

//q 2.4
//g is tail recursive since the call to g is the last operation in the branch.

//let pi = g [1; 2; 3] [3; 4; 5]
(*
    The first recursive call to g is made with xs = [1; 2; 3] and ys = [3; 4; 5].
    Inside the first branch, g calls f 3 [1; 2; 3], which returns Some [2; 3]. 
    The pi is then mapped to g [2; 3] [4; 5] using Option.map.
    In the second recursive call to g, the inputs are xs = [2; 3] and ys = [4; 5]. 
    Inside the first branch, g calls f 4 [2; 3], which returns None. 
    Since the pi is None, the Option.map returns None as well. 
    Finally, Option.default_value replaces the None with false.
    The final pi of the evaluation is false, indicating that the g function returns false for the given inputs.
*)


let fTail x lst =
  let rec loop acc = function
    | [] -> acc None
    | y::ys when x = y -> acc (Some ys)
    | y::ys -> loop (fun res -> acc (Option.map (fun ys' -> y::ys') res)) ys
  loop (fun res -> res) lst

//--------------------------------------------------------------------------------------------------------
//q3

//q3.1

open System

let calculatePi (x : uint64) : decimal =
    let rec loop (x : uint64) (pi : decimal) (count : decimal) (divisor : decimal) : decimal =
        if x = 0UL then
            pi
        else
            let product = decimal(4) / (decimal(2) * divisor * (decimal(2) * divisor + decimal(1)) * (decimal(2) * divisor + decimal(2)))
            let newPi = pi + (count * product)
            let newCount = -count
            let newDivisor = divisor + decimal(1)    
            loop (x - 1UL) newPi newCount newDivisor
    
    loop x (decimal(3)) (decimal(1)) (decimal(1))

// let rec calculatePi x : decimal =
//     let rec loop i pi count = 
//         if i > x then
//             pi
//         else
//             let numerator = decimal(4)
//             let denominator = decimal((2UL * i) * (2UL * i + 1UL) * (2UL * i + 2UL))
//             let nextPi = pi + ((numerator / denominator) * count)
//             let nextCount = -count
//             loop(i + 1UL) nextPi nextCount
//     loop 1UL (decimal(3)) (decimal(1))

calculatePi 0UL  // = 3.0M
calculatePi 1UL  // = 3.1666666666666666666666666667M
calculatePi 42UL // = 3.1415895113348010771053921701M
calculatePi 10000000UL // = 3.1415926535897932384623932776M


//q3.2

let piSeq : seq<decimal> =
    let rec loop count list =
        let pi = calculatePi count
        let updatedList = Seq.append list (Seq.singleton pi)
        if count < 1000UL then
            loop (count + 1UL) updatedList
        else
            updatedList
    loop 0UL Seq.empty

let list = Seq.item 100 piSeq
//q3.3
let circleArea (r: float) =
    Seq.map (fun pi -> pi * decimal(r * r)) piSeq

let sphereVolume (r: float) =
    Seq.map (fun pi -> ((decimal(4)/decimal(3)) * pi) * decimal(r * r * r)) piSeq

circleArea 2.5   // = seq [18.750M; 19.79166667M; 19.58333333M; 19.6577381M; ...]
sphereVolume 2.5 // = seq [62.50M; 65.97222222M; 65.27777778M; 65.52579365M; ...]

//q3.4

let circleSphere (r: float) =
    let pairs = seq {
        for pi in piSeq do
            let area = pi * decimal(r * r)
            let volume = pi * decimal(r * r * r) * decimal(4.0) / decimal(3.0)
            yield (area, volume)
        }
    pairs

circleSphere 2.5 // = seq[(18.750M, 62.50M); (19.79166667M, 65.97222222M); (19.58333333M, 65.27777778M);(19.6577381M, 65.52579365M); ...] 

//q3.5

let calculatePartialPi (x : uint64) : decimal =
    let rec loop (x : uint64) (pi : decimal) (count : decimal) (divisor : decimal) : decimal =
        if x = 0UL then
            pi
        else
            let product = decimal(4) / (decimal(2) * divisor * (decimal(2) * divisor + decimal(1)) * (decimal(2) * divisor + decimal(2)))
            let newPi = pi + (count * product)
            let newCount = -count
            let newDivisor = divisor + decimal(1)    
            loop (x - 1UL) newPi newCount newDivisor
    
    loop x (decimal(0)) (decimal(1)) (decimal(1))

let calculateParallelPi (nop: uint64) (ipp: uint64) =
    async {
        let tasks =
            [
            async{
                let num =
                    if nop = 0UL || ipp = 0UL then
                        0UL
                    else
                    (nop * ipp) + 1UL
                let result = calculatePartialPi num
                return result
            }
            ]
        let! results = Async.Parallel tasks
        let sum = Array.sum results + 3.0M
        return sum
    }

let parallelPi nop ipp =
    Async.RunSynchronously (calculateParallelPi nop ipp)


calculatePartialPi 0UL

parallelPi 1UL 0UL       //3.000000000000000
                                    //3.0M
parallelPi 1UL 1000000UL //3.141592653589793
                                    //3.1415926535897932387126418813M
parallelPi 10UL 100000UL //3.141592653589793
                                    //3.1415926535897932387126418813M
parallelPi 100UL 10000UL //3.141592653589793
                                    //3.1415926535897932387126418813M

//-------------------------------------------------------------------------------------------------------------

//Q4

//q4.1

type Dir = Left | Right

type Tape<'a> =
    { tape: 'a array
      head: int }

let tapeFromList (osyms: 'a option list) : Tape<'a> =
    let tapeArray =
        Array.ofList osyms
        |> Array.choose id
    { tape = tapeArray
      head = 0 }

tapeFromList [Some true; Some true; Some true; Some false; 
              Some true; Some true; Some true; Some true; Some true; Some false]

let tapeToList (tape: Tape<'a>) : 'a list =
    let rec trimLeadingBlanks (arr: 'a array) : 'a array =
        match arr with
        | [||] -> [||]
        | [| blank |] when blank = Unchecked.defaultof<'a> -> [||]
        | _ -> arr

    let trimmedTape = trimLeadingBlanks tape.tape
    trimmedTape |> Array.toList

let moveHead (dir: Dir) (tape: Tape<'a>) : Tape<'a> =
    match dir with
    | Left -> { tape with head = tape.head - 1 }
    | Right -> { tape with head = tape.head + 1 }

let readTape (tape: Tape<'a>) : 'a option =
    let tapeArray = tape.tape

    if tape.head >= 0 && tape.head < Array.length tapeArray then
        Some tapeArray.[tape.head]
    else
        None

let writeTape (osym: 'a option) (tape: Tape<'a>) : Tape<'a> =
    let tapeArray = tape.tape
    let newArray =
        tapeArray
        |> Array.mapi (fun i sym ->
            if i = tape.head then
                Option.defaultValue sym osym
            else
                sym
        )

    { tape with tape = newArray }

