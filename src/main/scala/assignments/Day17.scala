package assignments

import scala.io.Source
import lib.Points.Point

object Day17:

  val rocks: Seq[Set[Point]] = Seq(
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),
    Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)),
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2)),
    Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)),
    Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1))
  )

  def canMove(rocks: Set[Point], grid: Set[Point]): Boolean =
    rocks.forall(p => p.x > 0 && p.x < 8 && !grid.contains(p))

  case class State(jets: String, grid: Set[Point], rockIndex: Int, jetIndex: Int, height: Int):
    def move(rock: Set[Point], jetIndex: Int): (Set[Point], Int) =
      val jet     = jets(jetIndex % jets.length)
      val blown   = if jet == '>' then rock.map(_ + Point(1, 0)) else rock.map(_ + Point(-1, 0))
      val blown2  = if canMove(blown, grid) then blown else rock
      val dropped = blown2.map(_ + Point(0, -1))
      if canMove(dropped, grid) then move(dropped, jetIndex + 1) else (blown2, jetIndex + 1)
    def step: State =
      val rock               = rocks(rockIndex % rocks.size).map(_ + Point(3, height + 4))
      val (rock2, jetIndex2) = move(rock, jetIndex)
      val height2            = height.max(rock2.map(_.y).max)
      State(jets, grid ++ rock2, rockIndex + 1, jetIndex2, height2)

  def tetris(jets: String): Iterator[Int] =
    val initial = State(jets, Set.tabulate(8)(Point(_, 0)), 0, 0, 0)
    Iterator.iterate(initial)(_.step).map(_.height)

  def findHeight(jets: String, n: Long): Long =
    val heights = tetris(jets).slice(1, 5 * 2022).toVector
    val delta   = heights.sliding(2).map(s => s.last - s.head).toSeq
    val end     = delta.size - 2022
    val start   = delta.lastIndexOfSlice(delta.takeRight(2022), end - 1)
    val (h, w)  = (heights(end) - heights(start), end - start)
    val offset  = n - 1 - start
    val (q, r)  = (offset / w, offset % w)
    (q * h) + heights(start + r.toInt)

  val input: String = Source.fromResource("day17.txt").mkString

  def partOne(): Int  = tetris(input).drop(2022).next
  def partTwo(): Long = findHeight(input, 1000000000000L)
