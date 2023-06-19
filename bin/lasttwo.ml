let rec lastwo = function
    | [] | [_] -> None
    | [ x; y ] -> Some (x, y)
    | _ :: t -> lastwo t;;

lastwo ["a"; "b"; "c"; "d"];;
