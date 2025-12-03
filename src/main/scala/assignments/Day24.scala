package assignments

import scala.annotation.{tailrec, targetName}
import scala.io.Source
import lib.Points.Point

object Day24:

  extension (a: Int)
    @targetName("mod plus")
    def %+(b: Int) =
      val remainder = (a - 1) % b
      remainder + 1 + (if remainder < 0 then b else 0)

  def cantMove(t: Int, valley: Seq[String])(p: Point): Boolean =
    !valley.indices.contains(p.y)
      || valley(p.y)(p.x) == '#'
      || valley((p.y + t) %+ height)(p.x) == '^'
      || valley((p.y - t) %+ height)(p.x) == 'v'
      || valley(p.y)((p.x + t) %+ width) == '<'
      || valley(p.y)((p.x - t) %+ width) == '>'

  @tailrec
  def search(points: Set[Point], t: Int = 0)(implicit exit: Point, valley: Seq[String]): Int =
    if points.contains(exit) then t
    else
      val next = points.flatMap(p => p.neighbors :+ p).filterNot(cantMove(t + 1, valley))
      search(next, t + 1)

  val input: Seq[String] = Source.fromResource("day24.txt").getLines.toSeq

  lazy val (width, height) = (input.head.length - 2, input.length - 2)
  val (entrance, exit)     = (Point(1, 0), Point(width, height + 1))
  val path1: Int           = search(Set(entrance))(using exit, input)

  def partOne(): Int = path1
  def partTwo(): Int =
    val path2 = search(Set(exit), path1)(using entrance, input)
    search(Set(entrance), path2)(using exit, input)
