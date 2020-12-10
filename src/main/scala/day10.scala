package day10

class Jolty(input: Iterator[String]) {
  val adapters = input.map(_.toInt).toIndexedSeq.sorted
  val device = adapters.max + 3

  def joltyDiff() : Map[Int, Int] = {
    var current = 0
    val device = adapters.max + 3

    (adapters :+ device).foldLeft(Map[Int, Int]()){ (map, rating) =>
      val diff = (rating - current).toInt
      current = rating
      val count = map.getOrElse(diff, 0) + 1
      map + (diff -> count)
    }
  }

  def validWays : Long = {
    val m = scala.collection.mutable.Map[Int, Long](0 -> 1L)
    (adapters :+ device).foreach(value => {
      val opts = (0 to 3)
        .map(offset => value - offset)
        .filter(n => n >= 0)
      m(value) = opts.map(n => m.getOrElse(n, 0L)).sum
    })

    m(device)
  }
}
