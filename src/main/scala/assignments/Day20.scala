package assignments

import scala.io.Source
import collection.mutable.ArrayBuffer

object Day20:

  case class Number(n: Long, i: Int)

  def findKey(xs: Seq[Number]): Long =
    val i0 = xs.indexWhere(_.n == 0)
    (1 to 3).map(n => xs((i0 + n * 1000) % xs.size).n).sum

  def mix(xs: Seq[Number], n: Int = 1): Long =
    val acc = ArrayBuffer.from(xs)
    (1 to n).foreach(_ =>
      xs.foreach(number =>
        val i = acc.indexOf(number)
        acc.remove(i)
        val i2 = (i + number.n) % acc.size
        acc.insert(i2.toInt + (if i2 < 0 then acc.size else 0), number)
      )
    )
    findKey(acc.toSeq)

  val input: List[Long] = Source.fromResource("day20.txt").getLines.map(_.toLong).toList

  def partOne(): Long = mix(input.zipWithIndex.map(Number.apply))
  def partTwo(): Long = mix(input.zipWithIndex.map((n, i) => Number(n * 811589153L, i)), 10)
