package assignments

import scala.io.Source
import lib.Points.Point
import lib.Graphs.bfs

object Day12:
  import lib.GridExtensions._

  val grid: Grid[Char] = Source.fromResource("day12.txt").mkString.toList.toGrid

  def neighbors(point: Point): List[Point] =
    def isDescendable(p1: Point, p2: Point) =
      def elevation(point: Point) = grid(point) match
        case 'S' => 'a'
        case 'E' => 'z'
        case c   => c
      elevation(p1) - elevation(p2) <= 1

    point.neighbors.filter(grid.contains).filter(isDescendable(point, _))

  def search(c: Char): Int =
    val end = grid.find(_._2 == 'E').get._1
    bfs.search(end)(neighbors)(grid(_) == c).get

  def partOne(): Int = search('S')
  def partTwo(): Int = search('a')
