package assignments

import scala.io.Source
import scala.annotation.tailrec

object Day05:
  import lib.MapExtensions._

  type Stacks = Map[Int, Seq[Char]]
  case class Move(n: Int, from: Int, to: Int)

  @tailrec
  def move(stacks: Stacks, moves: Seq[Move])(implicit order: Seq[Char] => Seq[Char]): Stacks =
    moves match
      case h :: t =>
        val crates     = order(stacks(h.from).takeRight(h.n))
        val (from, to) = (h.from -> stacks(h.from).dropRight(h.n), h.to -> (stacks(h.to) ++ crates))
        move(stacks + from + to, t)
      case _ => stacks

  def topText(stacks: Stacks): String = stacks.toSeq.sortBy(_._1).map(_._2.last).mkString

  private val regex = """move (\d+) from (\d) to (\d)""".r
  def parse(s: String): Move = s match
    case regex(n, from, to) => Move(n.toInt, from.toInt, to.toInt)

  val input: Seq[String] = Source.fromResource("day05.txt").getLines.toSeq

  val stacks: Stacks = input
    .takeWhile(_.exists(_.isLetter))
    .map(_.zipWithIndex.filter(_._1.isLetter).map(_.swap).toMap)
    .foldLeft(Map.empty[Int, Seq[Char]])(_ addOrUpdate _)
    .map(kv => kv._1 / 4 + 1 -> kv._2.reverse)
  val moves: Seq[Move] = input.dropWhile(_.nonEmpty).tail.map(parse)

  def partOne(): String = topText(move(stacks, moves)(_.reverse))
  def partTwo(): String = topText(move(stacks, moves))
