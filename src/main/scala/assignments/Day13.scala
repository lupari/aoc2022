package assignments

import scala.annotation.tailrec
import scala.io.Source

object Day13:

  def span(s: String): (String, String) = s.span(_.isDigit)

  @tailrec
  def compare(left: String, right: String): Boolean = (left, right) match
    case (l, r) if l.isEmpty || r.isEmpty => l.nonEmpty
    case (l, r) if l.head.isDigit && r.head.isDigit =>
      val ((lnum, lrest), (rnum, rrest)) = (span(l), span(r))
      if lnum == rnum then compare(lrest, rrest) else lnum.toInt < rnum.toInt
    case (l, r) if l.head == r.head => compare(l.tail, r.tail)
    case _ =>
      (left.head, right.head) match
        case (']', _) => true
        case (_, ']') => false
        case ('[', _) => val (num, rest) = span(right); compare(left, s"[$num]" ++ rest)
        case (_, '[') => val (num, rest) = span(left); compare(s"[$num]" ++ rest, right)

  val input: List[String] = Source.fromResource("day13.txt").getLines.filterNot(_.isEmpty).toList

  def partOne(): Int =
    val comparisons = input.grouped(2).map(i => compare(i.head, i.last))
    comparisons.zipWithIndex.filter(_._1 == true).map(_._2 + 1).sum

  def partTwo(): Int =
    val (dpkg2, dpkg6) = ("[[2]]", "[[6]]")
    val sorted         = (input :+ dpkg2 :+ dpkg6).sortWith(compare)
    (sorted.indexOf(dpkg2) + 1) * (sorted.indexOf(dpkg6) + 1)
