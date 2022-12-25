package assignments

import scala.io.Source
import scala.annotation.tailrec
import lib.Points.Point

object Day15:

  case class Interval(min: Int, max: Int):
    val size: Int = max - min + 1
    def merge(other: Interval): Seq[Interval] =
      if min <= other.max then Seq(Interval(other.min, other.max max this.max))
      else Seq(this, other)

  case class Data(sensor: Point, beacon: Point):
    val distance: Int = sensor.manhattan(beacon)
    def intervalAt(y: Int): Option[Interval] =
      val dy = (y - sensor.y).abs
      val dx = distance - dy
      if dx > -1 then
        val (left, right) = (sensor.x - dx, sensor.x + dx)
        val min           = if left == beacon.x then left + 1 else left
        val max           = if right == beacon.x then right - 1 else right
        if max > min then Some(Interval(min, max)) else None
      else None

  def merge(intervals: Seq[Interval]): Seq[Interval] =
    val sorted = intervals.sortBy(_.min)
    sorted.tail.foldLeft(Seq(sorted.head))((acc, i) => i.merge(acc.head) ++ acc.tail)

  def find(data: Seq[Data], max: Int): Seq[Point] =
    val beacons = data.map(_.beacon).toSet
    (0 to max).flatMap(y =>
      merge(data.flatMap(_.intervalAt(y))) match
        case _ :: i :: Nil =>
          val p = Point(i.max + 1, y)
          if 0 <= p.x && p.x <= max && !beacons.contains(p) then Some(p) else None
        case _ => None
    )

  private val regex = """.*x=(-?\d+), y=(-?\d+).*x=(-?\d+), y=(-?\d+)""".r
  def parse(line: String): Data = line match
    case regex(x1, y1, x2, y2) => Data(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))

  val data: List[Data] = Source.fromResource("day15.txt").getLines.toList.map(parse)

  def partOne(): Int = merge(data.flatMap(_.intervalAt(2000000))).map(_.size).sum
  def partTwo(): Long =
    val beacon = find(data, 4000000).head
    4000000L * beacon.x + beacon.y
