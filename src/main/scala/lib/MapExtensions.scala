package lib

object MapExtensions:

  extension [A, B](map: Map[A, Seq[B]])
    def addOrUpdate(other: Map[A, B]): Map[A, Seq[B]] =
      val (existing, absent) = other.partition(o => map.contains(o._1))
      val map2               = map ++ absent.map(kv => kv._1 -> Seq(kv._2))
      existing.foldLeft(map2)((acc, xs) => acc + (xs._1 -> (acc(xs._1) :+ xs._2)))
