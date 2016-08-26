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
  | Epsilon   -> true
  | Phi       -> false
  | Letter _  -> false
  | Seq (r,s) -> nullable r && nullable s
  | Alt (r,s) -> nullable r || nullable s
  | Star _    -> true

(* a rather simple-minded pretty printer *)
let rec pretty_print r = match r with
  | Epsilon   -> ""
  | Phi       -> "phi"
  | Letter c  -> String.make 1 c
  | Seq (r,s) -> String.concat "" ["("; pretty_print r; " "; pretty_print s; ")"]
  | Alt (r,s) -> String.concat "" ["("; pretty_print r; "+"; pretty_print s; ")"]
  | Star r    -> String.concat "" ["("; pretty_print r; "*"; ")"]

(* Task1: Implement the derivative operation as discussed in class *)
let rec deriv r c = match r with
  | Epsilon   -> Phi
  | Phi       -> Phi
  | Letter y  -> if y = c then Epsilon else Phi
  | Seq (r,s) -> if nullable r then Alt (Seq (deriv r c, s), deriv s c)
                 else Seq (deriv r c, s)
  | Alt (r,s) -> Alt (deriv r c, deriv s c)
  | Star r    -> Seq (deriv r c, Star r)

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
  | _       -> [r];;

(* convert type re to type internal_re *)
let rec convert_to_internal r = match r with
  | Epsilon   -> Epsilon_I
  | Phi       -> Phi_I
  | Letter r  -> Letter_I r
  | Seq (r,s) -> Seq_I (convert_to_internal r, convert_to_internal s)
  | Alt (r,s) -> let left = convert_to_internal r in
                 let right = convert_to_internal s in
                 let left_list = test_alt_list left in
                 let right_list = test_alt_list right in
                     Alt_I (List.append left_list right_list)
  | Star r    -> Star_I (convert_to_internal r)

(* helper function to remove duplicates *)
let uniq_cons x xs = if List.mem x xs then xs else x :: xs
let removeDuplicates xs = List.fold_right uniq_cons xs []

(* remove duplicates from the regular expression *)
let rec remove_duplicate r = match r with
  | Seq_I (r,s) -> Seq_I (remove_duplicate r, remove_duplicate s)
  | Star_I r    -> Star_I (remove_duplicate r)
  | Alt_I r     -> Alt_I (removeDuplicates (remove_duplicate_list r))
  | _           -> r
and remove_duplicate_list l = match l with
  | []      -> []
  | x :: xs -> (remove_duplicate x) :: (remove_duplicate_list xs)

(* convert type internal_re back to type re *)
let rec convert_to_re r = match r with
  | Epsilon_I   -> Epsilon
  | Phi_I       -> Phi
  | Letter_I r  -> Letter r
  | Seq_I (r,s) -> Seq (convert_to_re r, convert_to_re s)
  | Alt_I r     -> recover_alt r
  | Star_I r    -> Star (convert_to_re r)
and recover_alt l = match l with
  | x :: [] -> convert_to_re x
  | x :: xs -> Alt (convert_to_re x, recover_alt xs)

(* simplify the regular expression *)
let rec simp r = match r with
  | Seq (r,s) -> Seq (simp r, simp s)
  | Star r    -> Star (simp r)
  | Alt _     -> let internal = convert_to_internal r in
                 let no_duplicate = remove_duplicate internal in
                     convert_to_re no_duplicate
  | _         -> r
