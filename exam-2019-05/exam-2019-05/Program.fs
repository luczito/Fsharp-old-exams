﻿type Peano =
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
    | S p -> S (add p b)  // If a is S p (successor of p), recursively call add on p and b, then construct the successor of the result.

let rec mult (a: Peano) (b: Peano) : Peano =
    match a with
    | O -> O  // If a is zero (O), return O as the product of zero and any number is zero.
    | S p -> add b (mult p b)  // If a is S p (successor of p), recursively call mult on p and b, then add b to the product of p and b.

let rec pow (a: Peano) (b: Peano) : Peano =
    match b with
    | O -> S O  // If b is zero (O), return S O as any number raised to the power of zero is 1.
    | S p -> mult a (pow a p)  // If b is S p (successor of p), recursively call pow on a and p, then multiply a with the result.

add  (S (S O)) (S (S (S O))) // S (S (S (S (S O))))
mult (S (S O)) (S (S (S O))) // S (S (S (S (S (S O)))))
pow  (S (S O)) (S (S (S O))) // S (S (S (S (S (S (S (S O))))))) 


let tailAdd a b = 
    let rec aux a b acc =
        match a with 
            | O -> acc
            | S p -> aux p b (S (acc))
    aux a b b
let tailaddResult = tailAdd (S (S O)) (S (S (S O)))

let tailMult a b = 
    let rec aux a b acc =
        match a with
            | O -> acc
            | S p -> aux p b (tailAdd b acc)
    aux a b O

let tailMultResult: Peano = tailMult (S (S O)) (S (S (S O))) // S (S (S (S (S (S O)))))

let tailPow a b =
    let rec aux a b acc = 
        match b with 
            | O -> acc
            | S p -> aux a p (tailMult a acc)
    aux a b (S O)

let tailPowResult = tailPow  (S (S O)) (S (S (S O))) // S (S (S (S (S (S (S (S O))))))) 

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

let loopAddResult = loopAdd (S (S O)) (S (S (S O)))

let loopMult a b =
    let addA x =
        loopAdd x a
    loop(fun acc -> addA acc) O b

let loopMultResult: Peano = loopMult (S (S O)) (S (S (S O))) // S (S (S (S (S (S O)))))

let loopPow a b = 
    let multA x = 
        loopMult x a
    loop(fun acc -> multA acc) (S O) b

let loopPowResult = loopPow  (S (S O)) (S (S (S O))) // S (S (S (S (S (S (S (S O))))))) 


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
//the error occurs because the function does not properly handle if the result is none

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

//let result = g [1; 2; 3] [3; 4; 5]
(*
    The first recursive call to g is made with xs = [1; 2; 3] and ys = [3; 4; 5].
    Inside the first branch, g calls f 3 [1; 2; 3], which returns Some [2; 3]. 
    The result is then mapped to g [2; 3] [4; 5] using Option.map.
    In the second recursive call to g, the inputs are xs = [2; 3] and ys = [4; 5]. 
    Inside the first branch, g calls f 4 [2; 3], which returns None. 
    Since the result is None, the Option.map returns None as well. 
    Finally, Option.default_value replaces the None with false.
    The final result of the evaluation is false, indicating that the g function returns false for the given inputs.
*)


let fTail x lst =
  let rec loop acc = function
    | [] -> acc None
    | y::ys when x = y -> acc (Some ys)
    | y::ys -> loop (fun res -> acc (Option.map (fun ys' -> y::ys') res)) ys
  loop (fun res -> res) lst
