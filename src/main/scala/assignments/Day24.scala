package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Points.Point

object Day24:

  extension (a: Int)
    def %+(b: Int) =
      val remainder = (a - 1) % b
      remainder + 1 + (if remainder < 0 then b else 0)

  def cantMove(t: Int, input: Seq[String])(p: Point): Boolean =
    !input.indices.contains(p.y)
      || input(p.y)(p.x) == '#'
      || input((p.y + t) %+ height)(p.x) == '^'
      || input((p.y - t) %+ height)(p.x) == 'v'
      || input(p.y)((p.x + t) %+ width) == '<'
      || input(p.y)((p.x - t) %+ width) == '>'

  @tailrec
  def search(points: Set[Point], time: Int = 0)(implicit end: Point, input: Seq[String]): Int =
    if points.contains(end) then time
    else
      val next = points.flatMap(p => p.neighbors :+ p).filterNot(cantMove(time + 1, input))
      search(next, time + 1)

  val input = Source.fromResource("day24.txt").getLines.toSeq

  lazy val (width, height) = (input.head.size - 2, input.size - 2)
  val (start, end)         = (Point(1, 0), Point(width, height + 1))
  val first                = search(Set(start))(end, input)

  def partOne(): Int = first
  def partTwo(): Int =
    val second = search(Set(end), first)(start, input)
    search(Set(start), second)(end, input)
