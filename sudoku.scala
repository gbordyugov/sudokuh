/*
 * Scala translation of Peter Norvig's sudoku solver
 *
 * http://norvig.com/sudoku.html
 */

/*
 * TODO:
 *  - eliminate
 *  - search
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

  def  all(bs: List[Boolean]): Boolean = bs.foldLeft(true )(_ && _)
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


  def parse(s: String): Option[Values] = {
    Folds.foldlO(squares.zip(s))(iniValues)
      { (v, p) => assign(v, p._1, p._2) }
  }


  def assign(v: Values, c: Cell, d: Digit): Option[Values] = 
    if (!digits.contains(d))
      Option(v)
    else {
      // println("assigning " + d + " to " + c)
      for {
        digits <- v.get(c)
        others =  digits.filterNot(_==d)
        indxes =  List.fill(others.length)(c)
        u <- Folds.foldlO(indxes.zip(others))(v)
          { (v, p) => eliminate(v, p._1, p._2) }
      } yield u
    }


  /*
   * work in progress
   */
  def eliminate(v: Values, c: Cell, d: Digit): Option[Values] = {
    // println("eliminating " + d + " from " + c)
    v.get(c).flatMap {
      digs => {
        if (!digs.contains(d))
          Option(v)
        else {
          val e = digs.filterNot(_==d)
          val u = v - c + (c -> e)
          e.toList match {
            case Nil       => None // eliminated too much
            case x::Nil    => peers.get(c).flatMap {
              pes => {
                val ps = pes.toList
                val ds = List.fill(ps.length)(x)
                Folds.foldlO(ps.zip(ds))(u) {
                  (v, p) => eliminate(v, p._1, p._2)
                }
              }
            }
            case otherwise => Option(u)
          }
        }
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
}


object Folds {
  def foldl[A,B](l: List[A]) (z: B) (f: (B, A) => B): B = {
    def g(x: A, k: B => B)(y: B): B = k(f(y, x))
    l.foldRight((x: B) => x) (g) (z)
  }

  def foldr[A,B](l: List[A]) (z: B) (f: (A, B) => B): B = {
    def g(k: B => B, x: A)(y: B): B = k(f(x, y))
    l.foldLeft((x: B) => x) (g) (z)
  }

  def foldlO[A,B](l: List[A]) (z: B)
                 (f: (B, A) => Option[B]): Option[B] = {
    def g(x: A, k: B => Option[B])(z: B): Option[B] =
      f(z, x).flatMap(k(_))
    l.foldRight((x: B) => Option(x))(g)(z)
  }

  def foldrO[A,B](l: List[A]) (z: B)
                 (f: (A, B) => Option[B]): Option[B] = {
    def g(k: B => Option[B], x: A)(z: B): Option[B] =
      f(x, z).flatMap(k(_))
    l.foldLeft((x: B) => Option(x))(g)(z)
  }
}

def goEasy() =
  println(SudokuSolver.valuesToString(SudokuSolver.parse(SudokuSolver.easyProblem)))
def goHard() =
  println(SudokuSolver.valuesToString(SudokuSolver.parse(SudokuSolver.hardProblem)))
