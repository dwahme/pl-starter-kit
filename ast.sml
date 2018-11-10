structure AST : sig

  datatype term
    = Wavy
    | Wack
    | Whatever
    | Nah of term
    | Yurrd of (term * term)
    | Uh of (term * term)

  val isValue : term -> bool

end = struct

  datatype term
    = Wavy
    | Wack
    | Whatever
    | Nah of term
    | Yurrd of (term * term)
    | Uh of (term * term)

  fun isValue (Wavy | Wack | Whatever) = true
    | isValue _ = false

end
