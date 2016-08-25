(* File re.ml *)

type re =
 | Epsilon
 | Phi
 | Letter of char
 | Seq of re * re
 | Alt of re * re
 | Star of re


(* check if epsilon in L(r) *)
let rec nullable r = match r with
  | Epsilon       -> true
  | Phi           -> false
  | Letter _      -> false
  | Seq (r,s)     -> nullable r && nullable s
  | Alt (r,s)     -> nullable r || nullable s
  | Star _        -> true

(* a rather simple-minded pretty printer *)
let rec pretty_print r = match r with
  | Epsilon       -> ""
  | Phi           -> "phi"
  | Letter c      -> String.make 1 c
  | Seq (r,s)     -> String.concat "" ["("; pretty_print r; " "; pretty_print s; ")"]
  | Alt (r,s)     -> String.concat "" ["("; pretty_print r; "+"; pretty_print s; ")"]
  | Star r        -> String.concat "" ["("; pretty_print r; "*"; ")"]

(* Task1: Implement the derivative operation as discussed in class *)

let rec deriv r c = match r with
      | Epsilon           -> Phi
      | Phi               -> Phi
      | Letter y          -> if y = c then Epsilon else Phi
      | Seq (r,s)         -> if nullable r then Alt (Seq (deriv r c, s), deriv s c)
                             else Seq (deriv r c, s)
      | Alt (r,s)         -> Alt (deriv r c, deriv s c)
      | Star r            -> Seq (deriv r c, Star r)

(* Task2: Implement the simplification rules *)

type internal_re =
    | Epsilon_I
    | Phi_I
    | Letter_I of char
    | Seq_I of internal_re * internal_re
    | Alt_I of (internal_re list)
    | Star_I of internal_re

let test_alt_list r = match r with
    | Alt_I r -> r
    | _ -> [r];;

let rec convert_to_internal r = match r with
      | Epsilon        -> Epsilon_I
      | Phi            -> Phi_I
      | Letter r       -> Letter_I r
      | Seq (r,s)      -> Seq_I (convert_to_internal r, convert_to_internal s)
      | Alt (r,s)      -> let left = convert_to_internal r in
                          let right = convert_to_internal s in
                          let left_list = test_alt_list left in
                          let right_list = test_alt_list right in
                              Alt_I (List.append left_list right_list)
      | Star r         -> Star_I (convert_to_internal r)

let uniq_cons x xs = if List.mem x xs then xs else x :: xs
let removeDuplicates xs = List.fold_right uniq_cons xs []

let rec remove_duplicate r = match r with
    | Seq_I (r,s)  -> Seq_I (remove_duplicate r, remove_duplicate s)
    | Star_I r     -> Star_I (remove_duplicate r)
    | Alt_I r      -> Alt_I (removeDuplicates (remove_duplicate_list r))
    | _ -> r
and remove_duplicate_list l = match l with
    | [] -> []
    | x :: xs -> (remove_duplicate x) :: (remove_duplicate_list xs)

let rec simp r = r

(* Hint: You might want to transform a re value into some internal
         form where alternatives are kept in a list. *)

