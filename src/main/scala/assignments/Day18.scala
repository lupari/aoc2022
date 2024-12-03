package assignments

import scala.io.Source
import lib.Graphs.floodfill

import scala.annotation.targetName

object Day18:

  case class Cube(x: Int, y: Int, z: Int):
    @targetName("add")
    def +(dx: Int, dy: Int, dz: Int): Cube = Cube(x + dx, y + dy, z + dz)
    def neighbors: Set[Cube] =
      Set((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)).map(+)

  def parse(line: String): Cube =
    val Seq(x, y, z) = line.split(",").map(_.toInt).toSeq
    Cube(x, y, z)

  def exteriorSurfaceArea(cubes: List[Cube]): Int =
    val xs    = cubes.map(_.x).min - 1 to cubes.map(_.x).max + 1
    val ys    = cubes.map(_.y).min - 1 to cubes.map(_.y).max + 1
    val zs    = cubes.map(_.z).min - 1 to cubes.map(_.z).max + 1
    val start = Cube(xs.head, ys.head, zs.head)
    def filter(c: Cube): Boolean =
      xs.contains(c.x) && ys.contains(c.y) && zs.contains(c.z) && !cubes.contains(c)
    val filled = floodfill(start, _.neighbors)(filter)
    cubes.flatMap(_.neighbors).count(filled.contains)

  val cubes: List[Cube] = Source.fromResource("day18.txt").getLines.toList.map(parse)

  def partOne(): Int = cubes.map(_.neighbors.filterNot(cubes.contains).size).sum
  def partTwo(): Int = exteriorSurfaceArea(cubes)
