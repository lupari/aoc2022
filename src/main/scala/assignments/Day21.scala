package assignments

import scala.annotation.tailrec
import scala.io.Source

object Day21:

  case class Operation(m1: String, m2: String, op: Char):
    def execute(a: Long, b: Long): Long = op match
      case '+' => a + b
      case '-' => a - b
      case '*' => a * b
      case '/' => a / b
  case class Monkey(name: String, n: Option[Long], op: Option[Operation])

  def parse(line: String): Monkey = line match
    case s"$name: $p1 $op $p2" => Monkey(name, None, Some(Operation(p1, p2, op.head)))
    case s"$name: $n"          => Monkey(name, Some(n.toInt), None)

  def findScore(monkeys: Map[String, Monkey], monkey: Monkey): Option[Long] =
    val op       = monkey.op.get
    val (m1, m2) = (monkeys(op.m1), monkeys(op.m2))
    if m1.n.isDefined && m2.n.isDefined then Some(op.execute(m1.n.get, m2.n.get)) else None

  def simulate(monkeys: Map[String, Monkey]): Map[String, Monkey] =
    @tailrec
    def helper(acc: Map[String, Monkey]): Map[String, Monkey] =
      val unresolved = acc.filter(kv => kv._2.op.isDefined)
      if unresolved.isEmpty then acc
      else
        val resolvable =
          unresolved.map(kv => (kv._1 -> findScore(acc, kv._2))).filter(_._2.isDefined)
        val updated =
          resolvable.map(kv => (kv._1 -> acc(kv._1).copy(op = None, n = Some(kv._2.get))))
        helper(acc ++ updated)

    helper(monkeys)

  def findSeed(monkeys: Map[String, Monkey]): Int =
    @tailrec
    def helper(i: Int, monkeys: Map[String, Monkey]): Int =
      println(i)
      val simulated = simulate(monkeys)
      val test      = (simulated(root.op.get.m1), simulated(root.op.get.m2))
      if test._1.n.get == test._2.n.get then i
      else helper(i + 1, monkeys.updated(you.name, you.copy(n = Some(i + 1))))

    val init = monkeys.updated(you.name, you.copy(n = Some(1))) - root.name
    helper(1, monkeys.updated(you.name, you.copy(n = Some(1))) - root.name)

  val input   = Source.fromResource("day21.txt").getLines().toList.map(parse)
  val monkeys = input.map(m => (m.name -> m)).toMap
  val you     = monkeys("humn")
  val root    = monkeys("root")

  def partOne(): Long = simulate(monkeys)(root.name).n.get
  def partTwo(): Int  = findSeed(monkeys)
