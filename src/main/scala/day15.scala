package day15



class NumberGame(initial: List[Long]) {
  var last : Map[Long, Long] = initial
    .zipWithIndex
    .map { case (number, turn) => (number -> (turn + 1).toLong) }
    .toMap

  var prev : Map[Long, Long] = Map()

  var current : Long = initial.last
  var turn : Long = initial.length + 1

  def step() : Long = {
    val lastSpoken = last.getOrElse(current, 0L)
    val firstSpoken = prev.get(current)
    val next = firstSpoken match {
      case None => 0
      case Some(v) => lastSpoken - v
    }
//    println(s"turn $turn: number is $current last spoken $lastSpoken before that $firstSpoken (diff = $next)")

    if (last.contains(next))
      prev = prev + (next -> last(next))

    last = last + (next -> turn)

    turn = turn + 1
    current = next
    next
  }
}
