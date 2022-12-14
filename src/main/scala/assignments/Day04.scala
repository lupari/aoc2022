package assignments

import scala.io.Source

object Day04:

  private val regex = """(\d+)-(\d+),(\d+)-(\d+)""".r
  def parse(line: String): (Range, Range) = line match
    case regex(a, b, c, d) => (a.toInt to b.toInt, c.toInt to d.toInt)

  def contained(r: (Range, Range)): Boolean  = r._1.containsSlice(r._2) || r._2.containsSlice(r._1)
  def intersects(r: (Range, Range)): Boolean = r._1.intersect(r._2).nonEmpty

  val ranges: Seq[(Range, Range)] = Source.fromResource("day04.txt").getLines.toList.map(parse)

  def partOne(): Int = ranges.count(contained)
  def partTwo(): Int = ranges.count(intersects)
