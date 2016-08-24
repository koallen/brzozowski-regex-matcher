(* File calc.ml *)
        open Re

        let rec matcher r w =
               if String.length w == 0
               then nullable r
               else matcher (simp (deriv r w.[0])) (String.sub w 1 ((String.length w) - 1))
               
        let _ =
          try
            let lexbuf = Lexing.from_channel stdin in
            while true do
              let result = Parser.main Lexer.token lexbuf in
                Printf.printf "%b" (nullable result); print_newline(); flush stdout
            done
          with Lexer.Eof ->
            exit 0
