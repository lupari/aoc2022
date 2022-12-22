package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.GridExtensions._
import lib.Points._

object Day22:

  trait Move
  case class Fwd(n: Int)   extends Move
  case class Turn(c: Char) extends Move

  def parse(dirs: String): List[Move] =
    @tailrec
    def helper(xs: String, acc: List[Move]): List[Move] =
      if xs.isEmpty then acc
      else if xs.head.isLetter then helper(xs.tail, acc :+ Turn(xs.head))
      else
        val (n, rest) = xs.span(_.isDigit)
        helper(rest, acc :+ Fwd(n.toInt))

    helper(dirs, List.empty)

  def step1(dir: Dir): Dir =
    val next = dir.forward(1)
    grid(next.p) match
      case '.' => next
      case '#' => dir
      case _ =>
        val otherSide = dir.dir match
          case 'U' => grid.filter((p, c) => p.x == dir.p.x && c != ' ').maxBy(_._1.y)
          case 'D' => grid.filter((p, c) => p.x == dir.p.x && c != ' ').minBy(_._1.y)
          case 'R' => grid.filter((p, c) => p.y == dir.p.y && c != ' ').minBy(_._1.x)
          case 'L' => grid.filter((p, c) => p.y == dir.p.y && c != ' ').maxBy(_._1.x)
        if otherSide._2 == '.' then dir.copy(p = otherSide._1) else dir

  def step2(dir: Dir): Dir =
    val next = dir.forward(1)
    grid(next.p) match
      case '.' => next
      case '#' => dir
      case _   =>
        //  AB
        //  C
        // ED
        // F
        val (divX, divY, modX, modY) = (dir.p.x / 50, dir.p.y / 50, dir.p.x % 50, dir.p.y % 50)
        val nextX                    = if next.p.x >= 0 then next.p.x / 50 else next.p.x
        val nextY                    = if next.p.y >= 0 then next.p.y / 50 else next.p.y
        val otherSide = (divX, divY, next.p.x / 50, next.p.y / 50) match
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

        if grid(otherSide.p) == '.' then otherSide else dir

  def move(moves: List[Move])(step: Dir => Dir): Int =
    @tailrec
    def helper(xs: List[Move], dir: Dir): Int = xs match
      case Fwd(x) :: t  => helper(t, Iterator.iterate(dir)(step).drop(x).next)
      case Turn(d) :: t => helper(t, dir.rotate(d))
      case _            => 1000 * (dir.p.y + 1) + 4 * (dir.p.x + 1) + "RDLU".indexOf(dir.dir)

    val start = grid.filter((p, c) => p.y == 0 && c == '.').toSeq.minBy(_._1.x)._1
    helper(moves, Dir(start, 'R'))

  val input       = Source.fromResource("day22.txt").getLines.toList
  val (map, dirs) = (input.dropRight(2), input.last)
  val grid        = map.mkString("\n").toList.toGrid.withDefaultValue(' ')
  val moves       = parse(dirs)

  def partOne(): Int = move(moves)(step1)
  def partTwo(): Int = move(moves)(step2)
