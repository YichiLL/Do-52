(* indent.ml contains useful methods used in scanner.mll that help implement
 * python-style indentation-based blocks. Indents can be made using actual
 * tab characters or spaces. A mix can even be used, but it would be
 * very confusing. *)

(* Converts a string to a list of char *)
let explode str =
    let rec exp len ls =
        if len < 0 then
            ls
        else
            exp (len - 1) (str.[len] :: ls)
    in
        exp (String.length str - 1) []

(* Returns the indentation level of the last line in the given string.
 * i.e., the number of tabs and spaces after the last '\n' *)
let rec depth_count count_so_far = function
    | [] -> count_so_far
    | c :: ls ->
        match c with
        | '\t'
        | ' ' -> depth_count (count_so_far + 1) ls
        | '\n' -> depth_count 0 ls
        | _ -> depth_count count_so_far ls
