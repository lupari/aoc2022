package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Points._

object Day22:
  import lib.GridExtensions._

  trait Move
  case class Fwd(n: Int)   extends Move
  case class Turn(c: Char) extends Move

  def parse(dirs: String): List[Move] =
    @tailrec
    def helper(xs: List[Char], acc: List[Move]): List[Move] = xs match
      case h :: t if h.isLetter => helper(t, acc :+ Turn(h))
      case _ :: _ =>
        val (n, rest) = xs.span(_.isDigit)
        helper(rest, acc :+ Fwd(n.mkString.toInt))
      case _ => acc

    helper(dirs.toList, List.empty)

  def step(dir: Dir)(leap: Dir => Dir): Dir =
    val next = dir.forward()
    grid(next.p) match
      case '.' => next
      case '#' => dir
      case _ =>
        val leaped = leap(dir)
        if grid(leaped.p) == '.' then leaped else dir

  def move(moves: List[Move])(leap: Dir => Dir): Int =
    @tailrec
    def helper(xs: List[Move], dir: Dir): Int = xs match
      case Fwd(x) :: t  => helper(t, Iterator.iterate(dir)(step(_)(leap)).drop(x).next)
      case Turn(d) :: t => helper(t, dir.rotate(d))
      case _            => 1000 * (dir.p.y + 1) + 4 * (dir.p.x + 1) + "RDLU".indexOf(dir.dir)

    val start = grid.filter((p, c) => p.y == 0 && c == '.').toSeq.minBy(_._1.x)._1
    helper(moves, Dir(start, 'R'))

  def leap1(dir: Dir): Dir =
    val otherSide = dir.dir match
      case 'U' => grid.filter((p, c) => p.x == dir.p.x && c != ' ').maxBy(_._1.y)
      case 'D' => grid.filter((p, c) => p.x == dir.p.x && c != ' ').minBy(_._1.y)
      case 'R' => grid.filter((p, c) => p.y == dir.p.y && c != ' ').minBy(_._1.x)
      case 'L' => grid.filter((p, c) => p.y == dir.p.y && c != ' ').maxBy(_._1.x)
    dir.copy(p = otherSide._1)

  def leap2(dir: Dir): Dir =
    //  AB
    //  C
    // ED
    // F
    val next                     = dir.forward()
    val (divX, divY, modX, modY) = (dir.p.x / 50, dir.p.y / 50, dir.p.x % 50, dir.p.y % 50)
    val nextX                    = if next.p.x >= 0 then next.p.x / 50 else next.p.x
    val nextY                    = if next.p.y >= 0 then next.p.y / 50 else next.p.y
    (divX, divY, nextX, nextY) match
      case (1, 0, 0, 0)  => Dir(Point(0, 149 - modY), 'R')     // A -> E
      case (1, 0, 1, -1) => Dir(Point(0, 150 + modX), 'R')     // A -> F
      case (1, 1, 2, 1)  => Dir(Point(100 + modY, 49), 'U')    // C -> B
      case (1, 1, 0, 1)  => Dir(Point(modY, 100), 'D')         // C -> E
      case (1, 2, 2, 2)  => Dir(Point(149, 49 - modY), 'L')    // D -> B
      case (1, 2, 1, 3)  => Dir(Point(49, 150 + modX), 'L')    // D -> F
      case (0, 2, -1, 2) => Dir(Point(50, 49 - modY), 'R')     // E -> A
      case (0, 2, 0, 1)  => Dir(Point(50, 50 + modX), 'R')     // E -> C
      case (2, 0, 2, 1)  => Dir(Point(99, 50 + modX), 'L')     // B -> C
      case (2, 0, 3, 0)  => Dir(Point(99, 149 - modY), 'L')    // B -> D
      case (2, 0, 2, -1) => dir.copy(p = Point(0 + modX, 199)) // B -> F
      case (0, 3, -1, 3) => Dir(Point(50 + modY, 0), 'D')      // F -> A
      case (0, 3, 0, 4)  => dir.copy(p = Point(100 + modX, 0)) // F -> B
      case (0, 3, 1, 3)  => Dir(Point(50 + modY, 149), 'U')    // F -> D
      case _             => next

  val input: List[String]    = Source.fromResource("day22.txt").getLines.toList
  val (map, dirs)            = (input.dropRight(2), input.last)
  val grid: Map[Point, Char] = map.mkString("\n").toList.toGrid.withDefaultValue(' ')
  val moves: List[Move]      = parse(dirs)

  def partOne(): Int = move(moves)(leap1)
  def partTwo(): Int = move(moves)(leap2)
