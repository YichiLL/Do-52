(* indent.ml contains useful methods used in scanner.mll that help implement
 * python-style indentation-based blocks. Indents can be made using actual
 * tab characters or spaces, but not a mix. *)

(* Converts a string to a list of char *)
let explode str =
    let rec exp len ls =
        if len < 0 then
            ls
        else
            exp (len - 1) (str.[len] :: ls)
    in
        exp (String.length str - 1) []

(* Returns the indentation level of a line. *)
let rec depth_count = function
    | [] -> 0
    | c :: ls ->
        if c == '\t' || c == ' ' then
            depth_count ls + 1
        else
            depth_count ls
            
