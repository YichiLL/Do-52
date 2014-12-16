open Parser

let rec build_list num_left ls =
    if num_left == 0 then
        ls
    else
        build_list (num_left - 1) (DEDENT :: ls)

(* Reads tokens from the scanner and passes them on to the parser. If a
 * DEDENT_MULT token is read, caches a number of DEDENT tokens equal to the
 * depth change, which are then each passed to the parser before going back
 * to getting tokens from the scanner. *)
let process =
    let cache = ref [] in (* This will be like a static var in the function. *)
    fun lexbuf ->
        match !cache with
        | tok::ls -> cache := ls; tok
        | [] -> match Scanner.token lexbuf with
            | DEDENT_MULT(diff) -> cache := build_list (diff - 1) []; DEDENT
            | tok -> tok
