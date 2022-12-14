package assignments

import scala.io.Source

object Day03:

  case class Item(label: Char):
    val priority: Int = label.toInt - (if label.isUpper then 38 else 96)
  case class Rucksack(compartment1: Set[Item], compartment2: Set[Item]):
    val error: Item      = (compartment1 & compartment2).head
    val items: Set[Item] = compartment1 ++ compartment2

  def findBadge(group: List[Rucksack]): Item = group.map(_.items).reduce(_ & _).head

  def parse(line: String): Rucksack =
    val (c1, c2) = line.splitAt(line.length / 2)
    Rucksack(c1.toSet.map(Item.apply), c2.toSet.map(Item.apply))

  val rucksacks: List[Rucksack] = Source.fromResource("day03.txt").getLines.map(parse).toList

  def partOne(): Int = rucksacks.map(_.error.priority).sum
  def partTwo(): Int = rucksacks.grouped(3).map(findBadge(_).priority).sum
