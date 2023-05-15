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

let tailMultResult = tailMult (S (S O)) (S (S (S O))) // S (S (S (S (S (S O)))))

let tailPow a b =
    let rec aux a b acc = 
        match b with 
            | O -> acc
            | S p -> aux a p (tailMult a acc)
    aux a b (S O)

let tailPowResult = tailPow  (S (S O)) (S (S (S O))) // S (S (S (S (S (S (S (S O))))))) 