object SudokuSolver {
  val testCell = ('C', '2')

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

  val units = squares.map(s => (s, unitlist.filter(_ contains s))).toMap
  val peers = squares.map(s => (s, (units(s).flatten.toSet - s))).toMap

  def  all(bs: List[Boolean]): Boolean = bs.foldLeft(true )(_ && _)
  def some(bs: List[Boolean]): Boolean = bs.foldLeft(false)(_ || _)

  def test() = {
    val cell = ('C', '2')
    assert(squares.length == 81)
    assert(unitlist.length == 27)
    assert(all(squares.map(s => units(s).length == 3)))
    assert(all(squares.map(s => peers(s).toList.length == 20)))
    assert(units(cell) ==
      List(
        List(('A','2'), ('B','2'), ('C','2'), ('D','2'), ('E','2'),
             ('F','2'), ('G','2'), ('H','2'), ('I','2')),
        List(('C','1'), ('C','2'), ('C','3'), ('C','4'), ('C','5'),
             ('C','6'), ('C','7'), ('C','8'), ('C','9')),
        List(('A','1'), ('A','2'), ('A','3'),
             ('B','1'), ('B','2'), ('B','3'),
             ('C','1'), ('C','2'), ('C','3'))))
    assert(peers(cell) ==
      Set(('A','2'), ('B','2'), ('D','2'), ('E','2'), ('F','2'),
          ('G','2'), ('H','2'), ('I','2'), ('C','1'), ('C','3'),
          ('C','4'), ('C','5'), ('C','6'), ('C','7'), ('C','8'),
          ('C','9'), ('A','1'), ('A','3'), ('B','1'), ('B','3')))
    println("All tests passed")
  }
}
