package assignments

import scala.io.Source
import scala.language.postfixOps
import lib.Graphs.bfs

object Day16:

  case class Valve(label: String, rate: Int, tunnels: List[String])

  def duration(v1: Valve, v2: Valve): Int =
    bfs.search(v1)(v => v.tunnels.map(valves(_)))(_ == v2).get + 1

  def build(v: Valve): (String, Map[String, Int]) =
    val m = valves.values.map(vv => vv -> duration(v, vv)).toMap
    (v.label, m.map(kv => kv._1.label -> kv._2))

  private val regex = """Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([\w, ]+)""".r
  def parse(line: String): (String, Valve) = line match
    case regex(l, r, ts) => (l, Valve(l, r.toInt, ts.split(", ").toList))

  val input: Seq[String]                       = Source.fromResource("day16.txt").getLines.toList
  val valves: Map[String, Valve]               = input.map(parse).toMap
  val durations: Map[String, Map[String, Int]] = valves.values.map(build).toMap
  val openValves: Set[String]                  = valves.filter(_._2.rate > 0).keySet

  def partOne(): Int =
    def find(valve: String, unseen: Set[String], time: Int, pressure: Int): Int = unseen
      .flatMap(v =>
        val timeLeft = time - durations(valve)(v)
        Option.when(timeLeft > 0)(
          find(v, unseen - v, timeLeft, pressure + timeLeft * valves(v).rate)
        )
      )
      .foldLeft(pressure)(_ max _)
    find("AA", openValves, 30, 0)

  def partTwo(): Int =
    case class State(valve1: String, valve2: String, unseen: Set[String], t1: Int, t2: Int)
    val memo = collection.mutable.Map[(Set[String], Set[String]), Int]().withDefaultValue(-1)
    def find(state: State, acc: Int): Int =
      val State(v1, v2, unseen, t1, t2) = state
      val key                           = (Set(v1, v2), unseen)
      if memo(key) >= acc then return -1 else memo(key) = acc
      val first = unseen.flatMap(next =>
        val timeLeft = t1 - durations(v1)(next)
        Option.when(timeLeft > 0)(
          find(State(next, v2, unseen - next, timeLeft, t2), acc + timeLeft * valves(next).rate)
        )
      )
      val second = unseen.flatMap(next =>
        val timeLeft = t2 - durations(v2)(next)
        Option.when(timeLeft > 0)(
          find(State(v1, next, unseen - next, t1, timeLeft), acc + timeLeft * valves(next).rate)
        )
      )
      (first ++ second).foldLeft(acc)(_ max _)

    find(State("AA", "AA", openValves, 26, 26), 0)
