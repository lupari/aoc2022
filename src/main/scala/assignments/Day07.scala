package assignments

import scala.io.Source

object Day07:
  import lib.MapExtensions._

  type Files = Map[String, Int]
  case class Dir(dirs: List[String], files: Files)

  def analyze(dir: Dir, line: String): Dir = line match
    case "$ cd .."      => dir.copy(dirs = dir.dirs.init)
    case s"$$ cd $name" => dir.copy(dirs = dir.dirs :+ dir.dirs.mkString + name)
    case s"$size $_" if size.head.isDigit =>
      dir.copy(files = dir.dirs.foldLeft(dir.files)((a, b) => a.addOrUpdate(b, size.toInt)))
    case _ => dir

  val root: Dir          = Dir(List("/"), Map.empty[String, Int])
  val input: Seq[String] = Source.fromResource("day07.txt").getLines.toSeq
  val files: Files       = input.foldLeft(root)(analyze).files

  def partOne(): Int = files.values.filter(_ < 100000).sum
  def partTwo(): Int = files.values.filter(_ >= 30000000 - (70000000 - files("/"))).min
