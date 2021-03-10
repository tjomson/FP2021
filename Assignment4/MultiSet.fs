module MultiSet
    type MultiSet<'a when 'a: comparison> = 
        | S of Map<'a, uint32>
        override this.ToString() = 
            match this with
                | S(m: Map<'a, uint32>) -> 
                    let asList: list<'a* uint32> = Map.toList m
                    let strings = Array.create (asList.Length * 2 - 1) ""
                    asList
                    |> List.iteri (fun i element ->
                        let key = element |> fst
                        let value = element |> snd
                        let toAdd = sprintf "(%A, #%A)" key (value |> int)
                        strings.[i * 2] <- toAdd
                        if i <> asList.Length - 1 then
                            strings.[i * 2 + 1] <- ", "
                    )
                    Array.fold (fun a b -> a + b) "" strings
                    |> fun a ->
                        sprintf "{%s}" a
            

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

    let fold (f: 'a -> 'b -> uint32 -> 'a) (acc: 'a) (S(m)) =
        Map.fold f acc m

    let foldBack (f: 'a -> uint32 -> 'b -> 'b) (S(m): MultiSet<'a>) (acc: 'b) = 
        Map.foldBack f m acc

    let map (f: 'a -> 'b) (S(m): MultiSet<'a>): MultiSet<'b> =
        let asList = Map.toList m
        let addToMap (map: Map<'b, uint32>) key value =
             defaultArg (map.TryFind(key)) 0u
            |> fun foundValue -> map.Add(key, foundValue + value)
        let rec mapper (addedTo: Map<'b, uint32>) (i: int) =
            match i with
                | -1 -> addedTo
                | n -> mapper (addToMap addedTo (asList.[i] |> fst|> f) (asList.[i] |> snd)) (n - 1)
            
        mapper Map.empty (asList.Length - 1) |> S


    let ofList (lst: 'a list) =
        let mutable set = empty
        lst
        |> List.iter (fun ele -> 
            set <- addSingle ele set 
        )
        set


    let toList (S(m)) =
        let rec addToList (list: 'a list) (thing: 'a) (i: uint32) =
            match i with
                | 0u -> list
                | n -> addToList ([thing] @ list) thing (i - 1u)

        Map.fold (fun (acc: 'a list) key value ->
             acc @ (addToList [] key value)
        ) [] m

    let union (S(m)) (S(k: Map<'a,uint32>)) =
        let leftJoin left (right: Map<'a,uint32>) =
            left
            |> Map.map (fun key value -> 
                defaultArg (right.TryFind(key)) 0u
                |> (fun i -> max i value)   
            )
        leftJoin m k
        |> leftJoin k
        |> S

  
    let sum (S(m)) (S(k: Map<'a,uint32>)) =
        let leftJoin left (right: Map<'a,uint32>) =
            left
            |> Map.map (fun key value -> 
                defaultArg (right.TryFind(key)) 0u
                |> (fun i -> i + value)   
            )
        let outerJoin left (right: Map<'a,uint32>) =
            left
            |> Map.map (fun key value -> 
                defaultArg (right.TryFind(key)) 0u
                |> (fun i -> 
                    if i > 0u then
                        i
                    else
                        value
                )   
            )
        leftJoin m k
        |> outerJoin k
        |> S

    let subtract ((S(m))) ((S(k))) =
        let keysToRemove = k |> Map.toList 
        let rec updateMap (resultMap: Map<'a, uint32>) (i: int) =
            match i with 
                | -1 -> resultMap
                | n -> 
                    let amountToRemove = k.Item(keysToRemove.[n] |> fst)
                    let amountPresent = defaultArg (m.TryFind(keysToRemove.[n] |> fst)) 0u
                    if amountToRemove >= amountPresent then
                        updateMap (resultMap.Remove(keysToRemove.[n] |> fst)) (n - 1)
                    else
                        updateMap (resultMap.Add (keysToRemove.[n] |> fst, (amountPresent - amountToRemove))) (n - 1)     
        updateMap m (keysToRemove.Length - 1)
        |> S


    let intersection (S(m)) (S(k)) =
        let keysToCheck = k |> Map.toList
        let rec updateMap (resultMap: Map<'a, uint32>) (i: int) =
            match i with
                | -1 -> resultMap
                | n ->
                    let mValue = defaultArg (m.TryFind(keysToCheck.[n] |> fst)) 0u
                    let kValue = keysToCheck.[n] |> snd
                    if mValue > 0u then
                        updateMap (resultMap.Add((keysToCheck.[n] |> fst), (min mValue kValue))) (n - 1)
                    else
                        updateMap resultMap (n - 1)
        updateMap Map.empty (keysToCheck.Length - 1)
        |> S
