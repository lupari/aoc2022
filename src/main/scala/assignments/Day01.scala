package assignments

import scala.io.Source

object Day01:

  val input = Source
    .fromResource("day01.txt")
    .mkString
    .split("\n\n")
    .map(_.split("\n").map(_.toInt))
    .map(_.sum)
    .sorted

  def partOne(): Int = input.last
  def partTwo(): Int = input.takeRight(3).sum
