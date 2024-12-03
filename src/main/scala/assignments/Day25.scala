package assignments

import scala.annotation.tailrec
import scala.io.Source

object Day25:

  def decode(snafu: String): Long =
    @tailrec
    def helper(xs: List[Char], acc: Long): Long = xs match
      case h :: t =>
        val n = h match
          case '=' => -2
          case '-' => -1
          case c   => c.asDigit
        helper(t, 5 * acc + n)
      case _ => acc
    helper(snafu.toList, 0)

  def encode(dec: Long): String =
    if dec == 0 then ""
    else
      val suffix = dec % 5 match
        case 3 => "="
        case 4 => "-"
        case x => x.toString
      encode((dec + 2) / 5) + suffix

  val input: List[String] = Source.fromResource("day25.txt").getLines.toList

  def partOne(): String = encode(input.map(decode).sum)
