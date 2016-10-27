object SudokuSolver {
  val rows = 'A' to 'I' toList
  val cols = '1' to '9' toList

  val digits = '1' to '9' toList

  def cross[A, B](as: List[A], bs: List[B]): List[(A, B)] = 
    for(a <- as; b <- bs) yield((a, b))

  val squares = cross(rows, cols)

  val unitlist = (for(c <- cols) yield(cross(rows,    List(c)))) ++
                 (for(r <- rows) yield(cross(List(r), cols   ))) ++
                 (for(r <- List("ABC", "DEF", "GHI");
                      c <- List("123", "456", "789"))
                  yield(cross(r.toList, c.toList)))

  val units = squares.map(s => (s, unitlist.filter(u => u contains s))).toMap
  val peers = squares.map(s => (s, units(s).flatten.toSet - List(s))).toMap
}
