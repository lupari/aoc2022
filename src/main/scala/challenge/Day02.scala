package challenge

import scala.io.Source

object Day02:

  val order1 = List("B X", "C Y", "A Z", "A X", "B Y", "C Z", "C X", "A Y", "B Z").zipWithIndex.toMap
  val order2 = List("B X", "C X", "A X", "A Y", "B Y", "C Y", "C Z", "A Z", "B Z").zipWithIndex.toMap  

  val input = Source.fromResource("day02.txt").getLines().toList

  def partOne(): Int = input.map(order1(_) + 1).sum
  def partTwo(): Int = input.map(order2(_) + 1).sum

