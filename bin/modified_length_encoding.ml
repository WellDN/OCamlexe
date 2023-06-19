type 'a tle =
    | One of 'a
    | Many of int * 'a;;

let encode l = 
    let create_tuple cnt elem =  
        if cnt = 1 then One elem
        else Many (cnt, elem) in
    let rec aux count acc = function
        | [] -> []
        | [x] -> (create_tuple (count + 1) x) :: acc
        | hd :: (snd :: _ as t1) ->
                if hd = snd then aux (count + 1) acc t1
                else aux 0 ((create_tuple (count + 1) hd ) :: acc ) t1 in
    List.rev (aux 0 [] l)
