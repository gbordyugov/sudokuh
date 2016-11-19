/*
 * Scala translation of Peter Norvig's sudoku solver
 *
 * http://norvig.com/sudoku.html
 */

/*
 * TODO:
 * - more elengat search()
 */

object Utils {

  def cross[A, B](as: List[A], bs: List[B]): List[(A, B)] =
    for(a <- as; b <- bs) yield((a, b))


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


  def all(bs: List[Boolean]): Boolean = bs.foldLeft(true )(_ && _)


  def msum[A](l: List[Option[A]]): Option[A] = l match {
    case Nil           => None
    case Some(x)::tail => Some(x)
    case None::tail    => msum(tail)
  }


  def noncrit[A](f: A => Option[A], a: A): Option[A] = f(a) match {
    case None    => Option(a)
    case Some(y) => Option(y)
  }
}


object Folds {
  def foldlO[A,B](l: List[A]) (z: B)
                 (f: (B, A) => Option[B]): Option[B] = {
    def g(x: A, k: B => Option[B])(z: B): Option[B] =
      f(z, x).flatMap(k(_))
    l.foldRight((x: B) => Option(x))(g)(z)
  }
}


object SudokuSolver {
  type Row = Char
  type Col = Char
  type Digit = Char
  type Cell = (Row, Col)
  type Square = Cell

  val testCell = ('C', '2')

  val rows = ('A' to 'I').toList
  val cols = ('1' to '9').toList

  val digits = "123456789"

  val squares: List[Cell] = Utils.cross(rows, cols)

  val unitlist = (for(c <- cols) yield(Utils.cross(rows,    List(c)))) ++
                 (for(r <- rows) yield(Utils.cross(List(r), cols   ))) ++
                 (for(r <- List("ABC", "DEF", "GHI");
                      c <- List("123", "456", "789"))
                  yield(Utils.cross(r.toList, c.toList)))

  val units = squares.map(s => (s, unitlist.filter(_ contains s))).toMap
  val peers = squares.map(s => (s, (units(s).flatten.toSet - s))).toMap

  def test() = {
    val cell = ('C', '2')
    assert(digits.length == 9)
    assert(squares.length == 81)
    assert(unitlist.length == 27)
    assert(Utils.all(squares.map(units(_).length == 3)))
    assert(Utils.all(squares.map(peers(_).toList.length == 20)))
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


  def parse(s: String): Option[Values] =
    Folds.foldlO(squares.zip(s))(iniValues)
      { (v, p) => assign(v, p._1, p._2) }


  def assign(v: Values, c: Cell, d: Digit): Option[Values] =
    if (!digits.contains(d)) Option(v)
    else removeFromPeers(v - c + (c -> d.toString), c, d)


  def dropDigit(v: Values, c: Cell, d: Digit): Option[Values] = {
    val others = v(c).filterNot{ _ == d }
    if (others.length == 0) None
    else Option(v - c + (c -> others))
  }


  def checkForSingleton(v: Values, c: Cell, d: Digit): Option[Values] =
    v(c).length match {
      case 0 => None
      case 1 => removeFromPeers(v, c, v(c).head)
      case _ => Option(v)
    }


  def removeFromPeers(v: Values, c: Cell, d: Digit): Option[Values] =
    Folds.foldlO(peers(c).toList.zip(List.fill(20)(d)))(v)
      { (v, p) => eliminate(v, p._1, p._2) }


  def eliminate(u: Values, c: Cell, d: Digit): Option[Values] = {
    if (!u(c).contains(d)) Option(u)
    else for {
      v <- dropDigit(u, c, d)
      w <- Utils.noncrit((x: Values) => checkForSingleton(x, c, d), v)
      z <- Utils.noncrit((x: Values) =>        checkUnits(x, c, d), w)
    } yield(z)
  }


  def checkUnits(v: Values, c: Cell, d: Digit): Option[Values] = {
    val jss = units(c).map(u => u.filter(c => v(c).contains(d)))
    Folds.foldlO(jss)(v) {
      (v, l) => l match {
        case Nil    => None
        case x::Nil => assign(v, x, d)
        case _      => Option(v)
      }
    }
  }


  def search(v: Option[Values]): Option[Values] = v match {
    case None    => None
    case Some(v) => {
      val ls = squares.filter(v(_).length > 1).map(s=>((v(s).length,s)))
      if (ls.isEmpty)
        Option(v)
      else {
        val (l, s) = ls.min
        Utils.msum(digits.toList.map(d => search(assign(v, s, d))))
      }
    }
  }


  def valuesToString(v: Option[Values]): String = v match {
    case None => new String("no solution")
    case Some(v) => {
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


  val easyProblem = "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."

  val hardProblem = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"


  def goEasy() = println(valuesToString(parse(easyProblem)))
  def goHard() = println(valuesToString(parse(hardProblem)))


  def solveEasy() = println(valuesToString(search(parse(easyProblem))))
  def solveHard() = println(valuesToString( search(parse(hardProblem))))

  def main(args: Array[String]): Unit = solveHard()
}
