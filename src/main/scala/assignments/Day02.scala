package assignments

import scala.io.Source

object Day02:

  val score1: Map[String, Int] =
    Seq("B X", "C Y", "A Z", "A X", "B Y", "C Z", "C X", "A Y", "B Z").zipWithIndex.toMap
  val score2: Map[String, Int] =
    Seq("B X", "C X", "A X", "A Y", "B Y", "C Y", "C Z", "A Z", "B Z").zipWithIndex.toMap

  val input: List[String] = Source.fromResource("day02.txt").getLines.toList

  def partOne(): Int = input.map(score1(_) + 1).sum
  def partTwo(): Int = input.map(score2(_) + 1).sum
