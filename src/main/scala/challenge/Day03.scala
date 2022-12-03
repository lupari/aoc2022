package challenge

import scala.io.Source

object Day03:

  case class Item(label: Char):
    val priority: Int = if label.isUpper then label.toInt - 38 else label.toInt - 96

  case class Rucksack(compartment1: Set[Item], compartment2: Set[Item]):
    val error: Item         = (compartment1 & compartment2).head
    val allItems: Set[Item] = compartment1 ++ compartment2

  def commonItem(rucksacks: List[Rucksack]): Item =
    val contents = rucksacks.map(_.allItems)
    contents.tail.foldLeft(contents.head)((a, b) => a & b).head

  def parse(line: String): Rucksack = Rucksack(
    line.take(line.size / 2).toSet.map(Item(_)),
    line.takeRight(line.size / 2).toSet.map(Item(_))
  )

  val rucksacks = Source.fromResource("day03.txt").getLines.map(parse).toList

  def partOne(): Int = rucksacks.map(_.error.priority).sum
  def partTwo(): Int = rucksacks.grouped(3).map(commonItem(_).priority).sum
