module MultiSet
    type MultiSet<'a when 'a: comparison> = S of Map<'a, uint32>
    
    let empty = S Map.empty
    
    let isEmpty (S (m)) = m.IsEmpty

    let size (S (m)) = 
        Map.fold (fun acc _ value -> acc + value) 0u m

    let contains a (S(m)) = m.ContainsKey a

    let numItems a (S(m)) = 
        defaultArg (m.TryFind(a)) 0u

    let add a n ((S(m))) =
        defaultArg (m.TryFind(a)) 0u
        |> fun value -> m.Add(a, n + value) |> S
        
        
    let addSingle a (S(m)) =
        add a 1u (m |> S)

    let remove a n (S(m)) =
        defaultArg (m.TryFind(a)) 0u
        |> fun value ->
            if (n >= value) then
                m.Remove(a) |> S
            else    
                m.Add(a, value - n) |> S
    
    let removeSingle a (S(m)) =
        remove a 1u (S(m))

    // let fold (f: 'a -> 'b -> uint32 -> 'a) (acc: 'a) (S(m)) = 

    // let foldBack (f: 'a -> uint32 -> 'b -> 'b) (S(m): MultiSet<'a>) (acc: 'b) = 

    // let map (f: 'a -> 'b) (S(m): MultiSet<'a>) (S(k): MultiSet<'b>) =

    // let ofList (lst: 'a list) =

    let toList (S(m)) =
        let rec addToList (list: 'a list) (thing: 'a) (i: uint32) =
            match i with
                | 0u -> list
                | n -> addToList ([thing] @ list) thing (i - 1u)

        Map.fold (fun (acc: 'a list) key value ->
            (addToList [] key value) @ acc
        ) [] m

    


    [<EntryPoint>]
    let main argv =
        printfn "Hello world"
        0 // return an integer exit code