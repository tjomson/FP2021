module Dict
open System.Collections.Generic
    type Dict =
        | D of Set<string>

    let empty () = D Set.empty
    
    let insert s (D(set)) = 
        set.Add(s)
        |> D

    let lookup s (D(set)) = 
        set.Contains s


    // type Dict =
    //     | Node of Dictionary<char, (bool * Dict)>

    // let empty () = Node (new Dictionary<char, (bool * Dict)>())

    // let insert (s: string) (Node(d)) =
    //     Seq.toList s
    //     |> List.iter (fun c ->

    //     )



    