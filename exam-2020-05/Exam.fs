module Exam2020
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020 = *)

(* 1: Insertion sort *)

(* Question 1.1 *)

    let rec insert x lst =
        match lst with
        | [] -> [x]
        | y::ys ->
            if x <= y then x :: lst
            else y :: (insert x ys)

    let rec insertionSort lst =
        match lst with
        | [] -> []
        | x::xs -> insert x (insertionSort xs)

(* Question 1.2 *)

    let insertTail x lst = 
        let rec loop x acc =
            function
            | [] -> acc ([x])
            | y::ys -> if x <= y then acc(x::ys) 
                        else loop x (fun acclst -> acc(x::acclst)) ys
        loop x (fun x -> x) lst  
        
    let insertionSortTail lst = 
        let rec loop acc = function
                | [] -> acc
                | x::xs -> loop (insert x acc) xs
        loop [] lst

(* Question 1.3 *)

    (* 
    Q: Why are the higher-order functions from the List library 
    not a good fit to implement insert?

    A: Because they work on the entire list rather than specific elements of the list.
    *)

    let insertionSort2 lst =
        let insertTail x lst = List.foldBack (fun y acc -> if x <= y then x::y::acc else y::acc) lst [x] in
        List.foldBack insertTail lst []


(* Question 1.4 *)

    let insertBy f x lst = 
        let rec loop lst =
            match lst with 
            |[] -> [x]
            | y::ys -> 
                if f x y then x::lst
                else y :: loop ys
        loop lst

    let rec insertionSortBy f lst = 
        match lst with 
        |[] -> []
        | x::xs -> insertBy f x (insertionSortBy f xs)

(* 2: Code Comprehension *)
    let rec foo x = 
        function 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

    let rec bar x =
        function
        | []        -> []
        | xs :: xss -> (x :: xs) :: bar x xss 

    let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: 
        foo: 'a -> 'a list -> 'a list
        bar: 'a -> 'a list list -> 'a list list
        baz: 'a list -> 'a list list


    Q: What do functions foo, bar, and baz do? 
       Focus on what they do rather than how they do it.

    A:
        foo: checks if x exists in a list, if yes then removes x from the list.
        bar: takes a list of lists and adds x to each of the inner lists.
        baz: performs a series of transformations on the input list xs by applying the foo, baz, and bar functions in a specific composition.


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: 
        foo: listContains
        bar: addToInner
        baz: transformList
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo generates a warning during compilation: 
    Warning: Incomplete pattern matches on this expression.

    
    Q: Why does this happen, and where? 

    A: because the base case of an empty list is not handled in the function.


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: No because baz checks for an empty list in both the inner and outer function, which makes it so that foo will never be called with nothing.

    *)

    let rec foo2 x = 
        function
        | [] -> [] 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

(* Question 2.3 *) 

    (* 
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: forward composition operator. It composes two functions in forward order.


    Q: What does it do? Focus on what it does rather than how it does it.

    A: Composes to functions in forward order. It takes two functions and combines them to one function.

    *)

(* Question 2.4 *)

    let bar2 x lst =
        List.map (fun inner -> inner @ [x]) lst 
        
(* Question 2.5 *)

    let baz2 _ = function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            List.fold (fun acc y -> (foo y >> baz >> bar y) xs @ acc) [] xs

(* Question 2.6 *)

    (*
    
    Q: The function foo is not tail recursive. Why?
    
    A: because the call to itself is not in the tail position as the last call made.

    *)

    let fooTail x lst = 
        let rec loop acc x lst =
            match lst with
            | [] -> List.rev acc
            | y :: ys when x = y -> List.append acc ys
            | y::ys -> loop (y :: acc) x ys
        loop [] x lst

(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

    type shape = unit (* replace unit with the correct type declaration *)
    type result = unit (* replace unit with the correct type declaration *)

    let rps _ = failwith "not implemented"

(* Question 3.2 *)

    type strategy = (shape * shape) list -> shape

    let parrot _ = failwith "not implemented"
    
    let beatingStrat _ = failwith "not implemented"

    let roundRobin _ = failwith "not implemented"

(* Question 3.3 *)

    (* 
    
    Q: It may be tempting to generate a function that calculates your 
       point tuple after n rounds and then use Seq.initInfinite to 
       generate the sequence. This is not a good solution. Why?

    A: <Your answer goes here>
    
    *)

    let bestOutOf _ = failwith "not implemented"

(* Question 3.4 *)

    let playTournament _ = failwith "not implemented"

(* 4: Revers Polish Notation *)

(* Question 4.1 *)

    type stack = unit (* replace unit with the correct type declaration *)

    let emptyStack = () (* replace () with the correct value *)

(* Question 4.2 *)

    type SM<'a> = S of (stack -> ('a * stack) option)

    let ret x = S (fun s -> Some (x, s))
    let fail  = S (fun _ -> None)
    let bind f (S a) : SM<'b> = 
        S (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (S g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (S f) = f emptyStack 

    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"

(* Question 4.3 *)

    let write str : SM<unit> = S (fun s -> printf "%s" str; Some ((), s))

    let read =
        let rec aux acc =
            match System.Console.Read() |> char with
            | '\n' when acc = [] -> None
            | c    when System.Char.IsWhiteSpace c -> 
                acc |> List.fold (fun strAcc ch -> (string ch) + strAcc) "" |> Some
            | c -> aux (c :: acc)

        S (fun s -> Some (aux [], s))

    (* 
    
    Q: Consider the definition of write There is a reason that the definition 
       is S (fun s -> printf "%s" str; Some ((), s)) and not just 
       ret (printf "%s" str). For a similar reason, in read, we write 
       S (fun s -> Some (aux [], s)) and not ret (aux []). 
       What is the problem with using ret in both of these cases?
    
    A: <Your answer goes here>
    
    *)

(* Question 4.4 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let calculateRPN _ = failwith "not implemented"