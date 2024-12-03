package assignments

import scala.io.Source
import scala.annotation.tailrec

object Day10:

  def step(sum: List[Int], cmd: String): List[Int] = cmd match
    case "noop"     => sum :+ sum.last
    case s"addx $n" => sum :+ sum.last :+ sum.last + n.toInt

  val input: List[String] = Source.fromResource("day10.txt").getLines.toList
  val sums: List[Int]     = input.foldLeft(List(1))(step)

  def partOne(): Int = sums.zipWithIndex.collect {
    case (sum, i) if (i - 19) % 40 == 0 => sum * (i + 1)
  }.sum

  def partTwo(): String = sums
    .grouped(40)
    .map(_.zipWithIndex.map((sum, i) => if (sum - i).abs <= 1 then '█' else ' ').mkString)
    .mkString("\n")
