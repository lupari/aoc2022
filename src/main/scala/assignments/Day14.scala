package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Points._

object Day14:

  type Cave = Set[Point]
  val movements: Seq[Point] = Seq(Point(0, 1), Point(-1, 1), Point(1, 1))

  @tailrec
  def drop(sand: Point)(implicit cave: Cave, bottom: Int): Point =
    if sand.y == bottom then sand
    else
      val positions = movements.map(sand + _).filterNot(cave.contains)
      if positions.isEmpty then sand else drop(positions.head)

  @tailrec
  def fill(cave: Cave)(implicit bottom: Int, isSettled: Point => Boolean): Int =
    val sand = drop(source)(cave, bottom)
    if isSettled(sand) then (cave -- rock).size else fill(cave + sand)

  def parse(pair: Seq[String]): Seq[Point] =
    val Seq(x1, y1) = pair.head.split(',').toSeq
    val Seq(x2, y2) = pair.last.split(',').toSeq
    Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)).points

  val input: Seq[Seq[String]] =
    Source.fromResource("day14.txt").getLines.toSeq.map(_.split(" -> ").toSeq)

  val rock: Cave    = input.flatMap(_.sliding(2).flatMap(parse)).toSet
  val floor: Int    = rock.maxBy(_.y).y
  val source: Point = Point(500, 0)

  def partOne(): Int = fill(rock)(floor, _.y == floor)
  def partTwo(): Int = fill(rock)(floor + 1, _ == source) + 1
