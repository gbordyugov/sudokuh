val rows = 'A' to 'I' toList
val cols = '1' to '9' toList

val digits = '1' to '9' toList

def cross[A, B](as: List[A], bs: List[B]): List[(A, B)] = 
  for(a <- as; b <- bs) yield((a, b))

val squares = cross(rows, cols)

val tmp = List(1, 2, 3) ++
          List(3, 2, 1)
/*
val unitlist = (for(c <- cols) yield(cross(rows,    List(c)))) ++
               (for(r <- rows) yield(cross(List(r), cols   )))
               */
