structure Scan : sig

  datatype token
    = Wavy (* True *)
    | Wack (* False *)
    | Whatever (* Maybe *)
    | Nah (* Not *)
    | Yurrd (* And *) 
    | Uh (* Or *)
    | LParen
    | RParen
    (* your concrete syntax here! *)

  val scan : string -> token list

end = struct

  datatype token
    = Wavy (* True *)
    | Wack (* False *)
    | Whatever (* Maybe *)
    | Nah (* Not *)
    | Yurrd (* And *) 
    | Uh (* Or *)
    | LParen
    | RParen
    (* your concrete syntax here! *)

  fun nextToken (cs : char list) : (token * char list) option =
    let 
      fun lp [] = NONE
        | lp (#"(" :: cs) = SOME (LParen, cs)
        | lp (#")" :: cs) = SOME (RParen, cs)
        | lp (#"W" :: #"a" :: #"v" :: #"y" :: cs) = SOME (Wavy, cs)
        | lp (#"W" :: #"a" :: #"c" :: #"k" :: cs) = SOME (Wack, cs)
        | lp (#"W" :: #"h" :: #"a" :: #"t" :: #"e" :: #"v" :: #"e" :: #"r" :: cs) = SOME (Whatever, cs)
        | lp (#"N" :: #"a" :: #"h" :: cs) = SOME (Nah, cs)
        | lp (#"Y" :: #"u" :: #"r" :: #"r" :: #"d" :: cs) = SOME (Yurrd, cs)
        | lp (#"U" :: #"h" :: cs) = SOME (Uh, cs)
        | lp ((#" " | #"\t" | #"\n") :: cs) = lp cs
        | lp _ = raise Fail ("scan error cannot scan" ^ (implode cs))
      in
        lp cs
      end
        

  fun scan program =
    let
      fun lp cs =
        (case nextToken cs
          of NONE => []
           | SOME (tok, cs) => tok :: (lp cs))
    in 
      lp (explode program)
    end

end
