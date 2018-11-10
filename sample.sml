
fun fact (n : int) : int = 
    if n = 0
    then 1
    else n * fact (n - 1)

fun mult (a : int, b : int) : int =
    if b = 1
    then a
    else a + mult (a, b - 1)

infix <*>
fun a <*> b = mult (a, b)

fun startsWithA s =
    (case (explode s)
        of (#"a" :: _) => true
         | (#"A" :: _) => true
         | _           => false)

fun intToOpt (i : int) : int option = 
    if i = 0
    then NONE
    else SOME i

(* fun fromOpt NONE = 0
  | fromOpt SOME n = n *)