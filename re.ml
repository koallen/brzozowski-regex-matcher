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
       (* complete the missing bits *)                                        

 (* Task2: Implement the simplification rules *)

let rec simp r = r 
       (* include the appropriate simplifications *)                                            
                               
 (* Hint: You might want to transform a re value into some internal
           form where alternatives are kept in a list. *)

 type internal_re =
     | Epsilon_I
     | Phi_I
     | Letter_I of char
     | Seq_I of re * re
     | Alt_I of (re list)
     | Star_I of re
