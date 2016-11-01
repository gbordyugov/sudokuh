/*
 * Scala translation of Peter Norvig's sudoku solver
 *
 * http://norvig.com/sudoku.html
 */

object Utils {
  def center(s: String, p: Int): String = {
    val l = s.length
    if (p <= l) s
    else {
      val nSpaces = p - l
      val pref: Int = nSpaces / 2
      var suff: Int = nSpaces - pref
      " "*pref + s + " "*suff
    }
  }

  def groupsOf[T](xs: List[T], n: Int) : List[List[T]] = xs match {
    case Nil => Nil
    case _   => xs.take(n) :: groupsOf(xs.drop(n), n)
  }
}

object SudokuSolver {
  type Row = Char
  type Col = Char
  type Digit = Char
  type Cell = (Row, Col)

  val testCell = ('C', '2')

  val rows = 'A' to 'I' toList
  val cols = '1' to '9' toList

  val digits = "123456789"

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
    assert(digits.length == 9)
    assert(squares.length == 81)
    assert(unitlist.length == 27)
    assert(all(squares.map(units(_).length == 3)))
    assert(all(squares.map(peers(_).toList.length == 20)))
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

  type PossibleDigits = String
  type Values = Map[Cell, PossibleDigits]

  val iniValues: Values = squares.map(s => (s -> digits)).toMap

  def parse(s: String):Option[Values] = ???
  def assign(v: Values, c: Cell, d: Digit): Option[Values] = ???
  def eliminate(v: Values, c: Cell, d: Digit): Option[Values] = ???

  def valuesToString(v: Values) = {
    val ss = squares.map(v(_))

    val width = 1 + ss.map(_.length).max
    val sep = "-"*width*3
    val hSep = "\n" + sep + "+" + sep + "+" + sep + "\n"

    val ps = ss.map(Utils.center(_, width))

    def combine[T](ls: List[T], s: String) =
      Utils.groupsOf(ls, 3).map(_.mkString(s))

    val hblocks = combine(ps, "")
    val lines   = combine(hblocks, "|")
    val vblocks = combine(lines, "\n")
    val grid    = combine(vblocks, hSep)

    grid.head
  }

}
