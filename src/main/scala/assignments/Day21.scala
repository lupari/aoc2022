package assignments

import scala.annotation.tailrec
import scala.io.Source

object Day21:

  case class Operation(m1: String, m2: String, op: Char):
    def execute(a: Double, b: Double): Double = op match
      case '+' => a + b
      case '-' => a - b
      case '*' => a * b
      case '/' => a / b
  case class Monkey(name: String, n: Option[Double], op: Option[Operation]):
    def resolve(v: Double): Monkey = copy(op = None, n = Some(v))

  def resolve(monkey: Monkey)(monkeys: Map[String, Monkey]): Option[(String, Double)] =
    monkey.op match
      case Some(op) =>
        (monkeys(op.m1).n, monkeys(op.m2).n) match
          case (Some(a), Some(b)) => Some((monkey.name, op.execute(a, b)))
          case _                  => None
      case _ => None

  @tailrec
  def doMonkeyMath(acc: Map[String, Monkey]): Map[String, Monkey] =
    val resolved = acc.values.flatMap(resolve(_)(acc))
    if resolved.isEmpty then acc
    else doMonkeyMath(acc ++ resolved.map((k, v) => k -> acc(k).resolve(v)))

  def binSearch(monkeys: Map[String, Monkey]): Double =
    val (l, r) = (root.op.get.m1, root.op.get.m2)

    @tailrec
    def helper(min: Double, max: Double): Double =
      val mid           = min + (max - min) / 2
      val next          = doMonkeyMath(monkeys + (human.name -> human.resolve(mid)))
      val (left, right) = (next(l).n.get, next(r).n.get)
      if left == right then mid
      else if left > right then helper(mid + 1, max)
      else helper(min, mid - 1)

    helper(1d, 5000000000000d)

  def parse(line: String): Monkey = line match
    case s"$name: $p1 $op $p2" => Monkey(name, None, Some(Operation(p1, p2, op.head)))
    case s"$name: $n"          => Monkey(name, Some(n.toDouble), None)

  val monkeys: Map[String, Monkey] =
    Source.fromResource("day21.txt").getLines().map(parse).map(m => m.name -> m).toMap
  val (root, human) = (monkeys("root"), monkeys("humn"))

  def partOne(): Double = doMonkeyMath(monkeys)(root.name).n.get
  def partTwo(): Double = binSearch(monkeys)
