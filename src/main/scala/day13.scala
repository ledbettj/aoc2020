package day13;

class Part1(input: Iterator[String]) {
  val timestamp = input.next().toLong
  val busses = input.next().split(",").filter(_ != "x").map(_.toLong)

  def nextDeparture() : Long = busses.minBy(waitTime)
  def waitTime(bus: Long) = (bus - timestamp % bus)

  def answer() : Long = {
    val d = nextDeparture()
    waitTime(d) * d
  }
}


class Part2(input: Iterator[String]) {
  val busses = input
    .drop(1)
    .next()
    .split(",")
    .zipWithIndex
    .filter { case (line, index) => line != "x" }
    .map { case (line, offset) => (line.toLong, offset.toLong) }
    .map {
      // need to convert into the appropriate form for CRT
      case (modulo, 0) => (modulo, 0L)
      case (modulo, offset) => (modulo, modulo - (offset % modulo))
    }

  // thanks https://www.dave4math.com/mathematics/chinese-remainder-theorem/
  def answer(): Long = {
    val n = busses.map(_._1)
    val a = busses.map(_._2)
    val N = n.reduce((a, b) => a * b)

    (0 until n.length).map { i =>
      val n_i = N / n(i)

      // now solve:
      // (n_i * u_i = 1) mod modulo
      var u_i = 0L
      while ((u_i * n_i) % n(i) != 1) {
        u_i += 1
      }

      println(s"${a(i)}, $n_i, $u_i");
      a(i) * n_i * u_i
    }.sum % N
  }

}
