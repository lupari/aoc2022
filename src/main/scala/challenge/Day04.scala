package challenge

import scala.io.Source

object Day04:

  private val regex = """(\d+)-(\d+),(\d+)-(\d+)""".r
  def parse(line: String): (Range, Range) = line match
    case regex(r1, r2, r3, r4) => ((r1.toInt to r2.toInt), (r3.toInt to r4.toInt))

  def contains(r1: Range, r2: Range): Boolean   = r1.containsSlice(r2) || r2.containsSlice(r1)
  def intersects(r1: Range, r2: Range): Boolean = r1.intersect(r2).nonEmpty

  val ranges = Source.fromResource("day04.txt").getLines.toList.map(parse)

  def partOne(): Int = ranges.count(r => contains(r._1, r._2))
  def partTwo(): Int = ranges.count(r => intersects(r._1, r._2))
