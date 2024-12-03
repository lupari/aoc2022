package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Points.{Dir, Point}

object Day09:

  def move(head: Point, dir: Char): Point = Dir(head, dir).forward().p

  def follow(p1: Point, p2: Point): Point = p1 - p2 match
    case Point(x, y) => if x.abs < 2 && y.abs < 2 then p2 else p2 + Point(x.sign, y.sign)

  def track(directions: List[Char], n: Int): Set[Point] =
    @tailrec
    def helper(xs: List[Char], seen: Set[Point], acc: Seq[Point]): Set[Point] = xs match
      case h :: t =>
        val knots = acc.tail.foldLeft(List(move(acc.head, h)))((a, b) => a :+ follow(a.last, b))
        helper(t, seen + knots(n - 1), knots)
      case _ => seen

    helper(directions, Set(Point.zero), Seq.fill(n)(Point.zero))

  def parse(line: String): List[Char] = List.fill(line.drop(2).toInt)(line.head)

  val input: List[Char] = Source.fromResource("day09.txt").getLines.flatMap(parse).toList

  def partOne(): Int = track(input, 2).size
  def partTwo(): Int = track(input, 10).size
