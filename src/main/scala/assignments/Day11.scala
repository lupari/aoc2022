package assignments

import scala.io.Source
import lib.Math.lcm

import scala.annotation.targetName
import scala.language.postfixOps

object Day11:
  type Op = Long => Long
  case class Test(mod: Int, ok: Int, nok: Int)
  case class Monkey(items: Seq[Long], op: Op, test: Test, count: Long = 0):
    def discard(): Monkey = copy(items = Seq(), count = count + items.size)
    @targetName("append") def :++(xs: Seq[Long]): Monkey = copy(items = items ++ xs)

  def parseOperation(line: String): Op = line.trim match
    case "Operation: new = old * old" => n => n * n
    case s"Operation: new = old * $v" => n => n * v.toLong
    case s"Operation: new = old + $v" => n => n + v.toLong

  def parseTest(lines: List[String]): Test =
    val mod     = lines.head.split(" ").last.toInt
    val success = lines.tail.head.last.asDigit
    val failure = lines.last.last.asDigit
    Test(mod, success, failure)

  def parseItems(line: String): List[Long] = line.trim match
    case s"Starting items: $s" => s.split(", ").map(_.toLong).toList

  def parse(lines: List[String]): Monkey =
    val items = parseItems(lines.tail.head)
    val op    = parseOperation(lines.drop(2).head)
    val test  = parseTest(lines.takeRight(3))
    Monkey(items, op, test)

  def step(monkeys: Seq[Monkey]): Seq[Monkey] =
    monkeys.indices.foldLeft(monkeys)((monkeys, i) =>
      val m         = monkeys(i)
      val (ok, nok) = m.items.map(m.op).partition(_ % m.test.mod == 0)
      monkeys
        .updated(i, m.discard())
        .updated(m.test.ok, monkeys(m.test.ok) :++ ok)
        .updated(m.test.nok, monkeys(m.test.nok) :++ nok)
    )

  def monkeyBusiness(monkeys: Seq[Monkey], n: Int)(fn: Op => Op): Long =
    Iterator
      .iterate(monkeys.map(m => m.copy(op = fn(m.op))))(step)
      .drop(n)
      .next
      .map(_.count)
      .sorted
      .takeRight(2)
      .product

  val input: Seq[Monkey] = Source
    .fromResource("day11.txt")
    .mkString
    .split("\n\n")
    .map(_.split("\n").toList)
    .map(parse)
    .toSeq

  def partOne(): Long =
    val devalue = (op: Op) => op.andThen(_ / 3)
    monkeyBusiness(input, 20)(devalue)
  def partTwo(): Long =
    val multiplier = input.map(_.test.mod.toLong).reduce(lcm)
    val devalue    = (op: Op) => op.andThen(_ % multiplier)
    monkeyBusiness(input, 10000)(devalue)
