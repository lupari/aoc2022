package assignments

import scala.annotation.tailrec
import scala.io.Source

object Day13:

  def parseNum(s: String): String = s.takeWhile(_.isDigit)

  @tailrec
  def compare(left: String, right: String): Boolean = (left, right) match
    case (l, r) if l.isEmpty || r.isEmpty => l.nonEmpty
    case (l, r) if l.head.isDigit && r.head.isDigit =>
      val (ln, rn) = (parseNum(l), parseNum(r))
      if ln == rn then compare(l.drop(ln.size), r.drop(rn.size)) else ln.toInt < rn.toInt
    case (l, r) if l.head == r.head => compare(left.tail, right.tail)
    case _ =>
      (left.head, right.head) match
        case (']', _) => true
        case (_, ']') => false
        case ('[', _) =>
          val num = parseNum(right)
          compare(left, s"[$num]" ++ right.drop(num.size))
        case (_, '[') =>
          val num = parseNum(left)
          compare(s"[$num]" ++ left.drop(num.size), right)

  val input = Source.fromResource("day13.txt").getLines.filterNot(_.isEmpty).toList

  def partOne(): Int =
    val comparisons = input.grouped(2).map(i => compare(i.head, i.last))
    comparisons.zipWithIndex.filter(_._1 == true).map(_._2 + 1).sum

  def partTwo(): Int =
    val (dpkg2, dpkg6) = ("[[2]]", "[[6]]")
    val sorted         = (input :+ dpkg2 :+ dpkg6).sortWith(compare)
    (sorted.indexOf(dpkg2) + 1) * (sorted.indexOf(dpkg6) + 1)
