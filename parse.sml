structure Parse : sig

  val parse : Scan.token list -> AST.term

end = struct

  structure S = Scan
  structure A = AST

  fun nextTerm (tokens : Scan.token list) : (AST.term * Scan.token list) option =
    let
      fun lp [] = NONE
        | lp (S.Wavy :: tks) = SOME (A.Wavy, tks)
        | lp (S.Wack :: tks) = SOME (A.Wack, tks)
        | lp (S.Whatever :: tks) = SOME (A.Whatever, tks)
        | lp (S.RParen :: tks) = NONE
        | lp (S.LParen :: S.Nah :: tks) = 
          (case (nextTerm tks)
            of SOME (term, (S.RParen :: tks')) => SOME (A.Nah (term), tks')
             | SOME _ => raise Fail ("ur dumb")
             | NONE => raise Fail ("ur dumber")
            ) 
        | lp (S.LParen :: S.Yurrd :: tks) = 
          (case (nextTerm tks)
            of SOME (term, (S.RParen :: tks')) => SOME (A.Nah (term), tks')
             | SOME _ => raise Fail ("ur dumb")
             | NONE => raise Fail ("ur dumber")
            ) 
        | lp _ = raise Fail ("Parse error: cannot parse")
    in 
      lp tokens
    end

  fun parse tokens =
    raise Fail ("Parse error: cannot parse")

end
