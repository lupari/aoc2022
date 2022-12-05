package assignments

import scala.io.Source
import scala.annotation.tailrec

object Day05:

  type Stacks = Map[Int, List[Char]]
  case class Move(n: Int, from: Int, to: Int)

  def move(stacks: Stacks, moves: Seq[Move])(sort: List[Char] => List[Char]): Stacks =
    @tailrec
    def helper(ms: Seq[Move], acc: Stacks): Stacks = ms match
      case Nil => acc
      case h :: t =>
        val crates = sort(acc(h.from).takeRight(h.n))
        helper(t, acc + (h.from -> acc(h.from).dropRight(h.n)) + (h.to -> (acc(h.to) ++ crates)))

    helper(moves, stacks)

  def topText(stacks: Stacks): String = stacks.toSeq.sortBy(_._1).map(_._2.last).mkString

  private val regex = """move (\d+) from (\d) to (\d)""".r
  def parse(s: String): Move = s match
    case regex(n, from, to) => Move(n.toInt, from.toInt, to.toInt)

  def add(stacks: Stacks, items: Map[Int, Char]): Stacks =
    val (existing, absent) = items.partition(i => stacks.contains(i._1))
    val stacks2            = stacks ++ absent.map(kv => kv._1 -> List(kv._2))
    existing.foldLeft(stacks2)((s, xs) => s + (xs._1 -> (s(xs._1) :+ xs._2)))

  val input = Source.fromResource("day05.txt").getLines.toList

  val stacks = input
    .takeWhile(_.contains('['))
    .map(_.zipWithIndex.filter(c => c._1.isUpper).map(c => c._2 -> c._1).toMap)
    .foldLeft(Map.empty[Int, List[Char]])(add)
    .map(kv => kv._1 / 4 + 1 -> kv._2.reverse)
  val moves = input.dropWhile(_.nonEmpty).tail.map(parse)

  def partOne(): String = topText(move(stacks, moves)(_.reverse))
  def partTwo(): String = topText(move(stacks, moves)(identity))
