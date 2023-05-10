type Peano =
    | O
    | S of Peano

let rec toInt (n: Peano) : uint32 =
    match n with
    | O -> 0u
    | S p -> 1u + toInt p

let rec fromInt (x: uint32) : Peano =
    match x with
    | 0u -> O
    | _ -> S (fromInt (x - 1u))

