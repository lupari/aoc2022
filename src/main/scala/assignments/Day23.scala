package assignments

import scala.io.Source
import lib.Points.Point

object Day23:
  import lib.GridExtensions._

  def propose(grove: Grid[Char], elf: Point, dirs: List[Char]): Option[(Point, Point)] =
    def check(fn: Point => Boolean, p: Point): Option[(Point, Point)] =
      if elf.surroundings.filter(fn).exists(grove(_) == '#') then None else Some(elf, elf + p)
    def next(d: Char) = d match
      case 'N' => check(_.y == elf.y - 1, Point(0, -1))
      case 'S' => check(_.y == elf.y + 1, Point(0, 1))
      case 'W' => check(_.x == elf.x - 1, Point(-1, 0))
      case 'E' => check(_.x == elf.x + 1, Point(1, 0))

    if elf.surroundings.exists(grove(_) == '#') then dirs.flatMap(next).headOption else None

  case class State(grove: Grid[Char], dirs: List[Char], finished: Boolean)
  def step(state: State): State =
    val elves       = state.grove.filter((_, c) => c == '#').keys.toList
    val proposals   = elves.flatMap(e => propose(state.grove, e, state.dirs))
    val occurrences = proposals.map(_._2).groupMapReduce(identity)(_ => 1)(_ + _)
    val doable      = proposals.filter(p => occurrences(p._2) == 1)
    val grove2      = state.grove ++ doable.map(_._1 -> '.') ++ doable.map(_._2 -> '#')
    State(grove2, state.dirs.tail :+ state.dirs.head, doable.isEmpty)

  def move: Iterator[State] = Iterator.iterate(State(grove, "NSWE".toList, false))(step)

  def vacancy(grove: Grid[Char]): Int =
    val elves = grove.filter(_._2 == '#')
    val (minX, maxX, minY, maxY) = (
      elves.keys.minBy(_.x).x,
      elves.keys.maxBy(_.x).x,
      elves.keys.minBy(_.y).y,
      elves.keys.maxBy(_.y).y
    )
    (for x <- minX to maxX; y <- minY to maxY yield Point(x, y)).count(grove(_) == '.')

  val grove: Grid[Char] =
    Source.fromResource("day23.txt").mkString.toList.toGrid.withDefaultValue('.')

  def partOne(): Int = vacancy(move.drop(10).next.grove)
  def partTwo(): Int = move.indexWhere(_.finished)
