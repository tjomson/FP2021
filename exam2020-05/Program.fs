// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// 1.1
let rec insert x lst =
    match lst with
    | [] -> 
        (x :: (lst |> List.rev)) |> List.rev
    | y :: ys -> 
        if (x <= y) then
            x :: (y :: ys)
        else
            y :: insert x ys

// let rec insertionSort lst =
//     List.fold (fun acc ele ->
//         insert ele acc
//     ) [] lst

let rec insertionSort lst =
    match lst with
    | [] -> []
    | x :: xs -> 
        insert x (insertionSort xs)

// 1.2
let insertTail x lst =
    let rec aux acc remain =
        match remain with
        | [] -> (x :: (acc |> List.rev)) |> List.rev
        | y :: ys -> 
            if (x <= y) then
                (x :: (y :: List.rev acc) |> List.rev)
            else
                aux ((y :: (lst |> List.rev)) |> List.rev) ys
    aux [] lst

[<EntryPoint>]
let main argv =
    0 // return an integer exit code