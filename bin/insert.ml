let rec insert_at x n = function
    | [] -> [x]
    | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n - 1) t;;
