package assignments

import scala.annotation.targetName
import scala.io.Source

object Day19:

  case class Costs(id: Int, ore1: Int, ore2: Int, ore3: Int, ore4: Int, clay: Int, obsidian: Int)
  case class Blueprint(ore: Int, clay: Int, obsidian: Int, geode: Int):
    @targetName("plus") def +(b: Blueprint): Blueprint =
      Blueprint(ore + b.ore, clay + b.clay, obsidian + b.obsidian, geode + b.geode)
    @targetName("minus") def -(b: Blueprint): Blueprint =
      Blueprint(ore - b.ore, clay - b.clay, obsidian - b.obsidian, geode - b.geode)
    @targetName("le") def <=(b: Blueprint): Boolean =
      ore <= b.ore && clay <= b.clay && obsidian <= b.obsidian && geode <= b.geode
  case class State(
      t: Int,
      bots: Blueprint,
      acc: Blueprint,
      ore: Boolean = false,
      clay: Boolean = false,
      obsidian: Boolean = false
  )

  def parse(l: String): Costs = """\d+""".r.findAllIn(l).toSeq match
    case Seq(id, o1, o2, o3, c, o4, obsidian) =>
      Costs(id.toInt, o1.toInt, o2.toInt, o3.toInt, o4.toInt, c.toInt, obsidian.toInt)

  def maximize(costs: Costs, t: Int): Int =
    val oreCost      = Blueprint(costs.ore1, 0, 0, 0)
    val clayCost     = Blueprint(costs.ore2, 0, 0, 0)
    val obsidianCost = Blueprint(costs.ore3, costs.clay, 0, 0)
    val geodeCost    = Blueprint(costs.ore4, 0, costs.obsidian, 0)
    val oreBot       = Blueprint(1, 0, 0, 0)
    val clayBot      = Blueprint(0, 1, 0, 0)
    val obsidianBot  = Blueprint(0, 0, 1, 0)
    val geodeBot     = Blueprint(0, 0, 0, 1)
    val maxOre       = List(costs.ore1, costs.ore2, costs.ore3, costs.ore4).max

    def helper(state: State): Int = state match
      case State(t, bots, acc, ore, clay, obsidian) =>
        if t == 0 then acc.geode
        else if geodeCost <= acc then helper(State(t - 1, bots + geodeBot, acc + bots - geodeCost))
        else
          def cmp(bot: Blueprint, cost: Blueprint, ok: Boolean): Option[State] =
            if ok then Some(State(t - 1, bots + bot, acc + bots - cost)) else None
          val ore2      = oreCost <= acc && bots.ore < maxOre
          val clay2     = clayCost <= acc && bots.clay < costs.clay
          val obsidian2 = obsidianCost <= acc && bots.obsidian < costs.obsidian
          val noOp      = State(t - 1, bots, acc + bots, ore2, clay2, obsidian2)
          val states = Seq(
            (oreBot, oreCost, ore2 && !ore),
            (clayBot, clayCost, clay2 && !clay),
            (obsidianBot, obsidianCost, obsidian2 && !obsidian)
          )
          (states.flatMap(cmp) :+ noOp).map(helper).max

    costs.id * helper(State(t, oreBot, Blueprint(0, 0, 0, 0)))

  val costs: List[Costs] = Source.fromResource("day19.txt").getLines().toList.map(parse)

  def partOne(): Int = costs.map(maximize(_, 24)).sum
  def partTwo(): Int = costs.take(3).map(maximize(_, 32)).product / 6
