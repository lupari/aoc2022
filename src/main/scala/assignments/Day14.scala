package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Points._

object Day14:

  type Cave = Set[Point]

  @tailrec
  def drop(sand: Point, cave: Cave)(implicit bottom: Int): Point =
    val moves = Seq(Point(0, 1), Point(-1, 1), Point(1, 1))
    if sand.y == bottom then sand
    else
      val next = moves.map(sand + _).filterNot(cave.contains)
      if next.isEmpty then sand else drop(next.head, cave)

  @tailrec
  def fill(cave: Cave)(implicit bottom: Int, isSettled: Point => Boolean): Int =
    val sand = drop(source, cave)(bottom)
    if isSettled(sand) then (cave -- rock).size else fill(cave + sand)

  def parse(pair: List[String]): Seq[Point] =
    val Seq(x1, y1) = pair.head.split(',').toSeq
    val Seq(x2, y2) = pair.last.split(',').toSeq
    Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)).points

  val input: List[List[String]] =
    Source.fromResource("day14.txt").getLines.toList.map(_.split(" -> ").toList)

  val rock: Cave    = input.flatMap(_.sliding(2).flatMap(parse)).toSet
  val floor: Int    = rock.maxBy(_.y).y
  val source: Point = Point(500, 0)

  def partOne(): Int = fill(rock)(floor, _.y == floor)
  def partTwo(): Int = fill(rock)(floor + 1, _ == source) + 1
