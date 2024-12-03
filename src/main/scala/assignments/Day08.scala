package assignments

import scala.annotation.tailrec
import scala.io.Source

import lib.Points.Point

object Day08:
  import lib.GridExtensions._

  def views(p: Point): Seq[Seq[Point]] =
    val north = (0 until p.y).reverse.map(Point(p.x, _))
    val south = (p.y + 1 to bounds._2).map(Point(p.x, _))
    val west  = (0 until p.x).reverse.map(Point(_, p.y))
    val east  = (p.x + 1 to bounds._1).map(Point(_, p.y))
    Seq(north, south, west, east)

  def isHidden(p: Point): Boolean =
    if p.x == 0 || p.y == 0 || p.x == bounds._1 || p.y == bounds._2 then false
    else views(p).map(_.exists(grid(_) >= grid(p))).forall(_ == true)

  def scenicScore(p: Point): Int =
    def score(trees: List[Int], tree: Int): Int =
      @tailrec
      def helper(xs: List[Int], acc: Int): Int = xs match
        case h :: t => if h < tree then helper(t, acc + 1) else acc + 1
        case _      => acc
      helper(trees, 0)

    views(p).map(r => score(r.map(grid(_)).toList, grid(p))).product

  val grid: Grid[Int]    = Source.fromResource("day08.txt").mkString.toList.toIntGrid
  val bounds: (Int, Int) = (grid.keys.maxBy(_.x).x, grid.keys.maxBy(_.y).y)

  def partOne(): Int = grid.keys.filterNot(isHidden).size
  def partTwo(): Int = grid.keys.map(scenicScore).max
