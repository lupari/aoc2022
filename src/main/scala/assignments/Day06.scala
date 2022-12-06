package assignments

import scala.io.Source

object Day06:

  def findMarker(s: String, n: Int): Int =
    s.zipWithIndex.sliding(n).find(_.map(_._1).toSet.size == n).get.last._2 + 1

  val input: String = Source.fromResource("day06.txt").mkString

  def partOne(): Int = findMarker(input, 4)
  def partTwo(): Int = findMarker(input, 14)
