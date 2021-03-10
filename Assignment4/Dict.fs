module Dict
    type Dict =
        | D of Set<string>

    let empty () = D Set.empty
    
    let insert s (D(set)) = 
        set.Add(s)
        |> D

    let lookup s (D(set)) = 
        let mutable found = false
        set |> Set.iter (fun ele -> 
            if ele = s then
                found <- true
        )
        found
