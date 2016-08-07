
//0
let add x y = 
    x + y

//1
let mult (x:float) (n:float) =
    let seed = Array.init (int n) (fun i -> x)
    seed |> Array.fold add 0.0

//2
let exp (x:float) (n:float) =
    let seed = Array.init (int n) (fun i -> x)
    seed |> Array.fold mult 1.0

//3
let exp2 (x:float) (n:float) =
    let seed = Array.init (int n) (fun i -> x)
    seed |> Array.fold exp x

//4
let exp3 (x:float) (n:float) =
    let seed = Array.init (int n) (fun i -> x)
    seed |> Array.fold exp2 x


let rec derp (l:int) (x:float) (n:float) =  //todo use int64? use unsigned?
    let seedState =
        function
        | 1 -> 0.0
        | 2 -> 1.0
        | _ -> float x

    match l with
    | 0 -> add (float x) (float n)
    | _ -> 
        let seed = Array.init (int n) (fun i -> x)  //todo, refactor to avoid array
        seed 
        |> Array.fold (derp (l-1)) (seedState l)


open System

Math.Pow(Math.Pow(3.0,3.0),3.0)

//> exp 16 16;;
//val it : int = 0
//> exp 4 4;;
//val it : int = 256
//> exp 5 5 ;;
//val it : int = 3125
//> exp 6 6;;
//val it : int = 46656
//> exp 7 7;;
//val it : int = 823543
//> exp 8 8;;
//val it : int = 16777216
//> exp 9 9;;
//val it : int = 387420489
//> exp 10 10;;
//val it : int = 1410065408
//> exp 11 11;;
//val it : int = 1843829075
//> exp 12 12;;
//val it : int = -251658240
//> exp 13 13;;
//val it : int = -1692154371
//> exp 14 14;;
//val it : int = -1282129920
//> exp 15 15;;
//val it : int = 1500973039
//> exp 16 16;;
//val it : int = 0
//> 
