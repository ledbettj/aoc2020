package day17

case class CubeGrid(grid: Map[(Int, Int, Int), Boolean]) {
  val xRange = Range(
    grid.keySet.minBy { case (x, y, z) => x }._1 - 1,
    grid.keySet.maxBy { case (x, y, z) => x }._1 + 1
  ).inclusive
  val yRange = Range(
    grid.keySet.minBy { case (x, y, z) => y }._2 - 1,
    grid.keySet.maxBy { case (x, y, z) => y }._2 + 1
  ).inclusive
  val zRange = Range(
    grid.keySet.minBy { case (x, y, z) => z }._3 - 1,
    grid.keySet.maxBy { case (x, y, z) => z }._3 + 1
  ).inclusive

  def activeCount : Int = grid.values.count(identity)

  def step() : CubeGrid = {
    var newGrid = grid;

    for(x <- xRange) {
      for(y <- yRange) {
        for(z <- zRange) {
          val pos = (x, y, z)
          val current = grid.getOrElse(pos, false)
          val neighbors = activeNeighbors(pos)
          val next = (current, neighbors) match {
            case (true, 2) | (true, 3) | (false, 3) => true
            case _ => false
          }
          if (next) {
            newGrid += (pos -> true)
          } else {
            newGrid -= pos
          }
        }
      }
    }

    CubeGrid(newGrid)
  }

  def activeNeighbors(pos: (Int, Int, Int)) : Int =  {
    var count = 0;
    for (x <- (-1 to 1)) {
      for (y <- (-1 to 1)) {
        for (z <- (-1 to 1)) {
          if (x != 0 || y != 0 || z != 0) {
            val neighbor = (pos._1 + x, pos._2 + y, pos._3 + z);
            if (grid.getOrElse(neighbor, false)) {
              count += 1
            }
          }
        }
      }
    }
    count
  }
}

object CubeGrid {
  def parse(input: Iterator[String]) : CubeGrid  = {
    val set = input
      .zipWithIndex
      .flatMap { case (line, y) =>
        line
          .zipWithIndex
          .filter { case (ch, x) => ch == '#' }
          .map { case (_, x) => ((x, y, 0) -> true) }
      }.toMap

    CubeGrid(set)
  }
}


// copy paste part 2 huzzah
case class TimeCubeGrid(grid: Map[(Int, Int, Int, Int), Boolean]) {
  val xRange = Range(
    grid.keySet.minBy { case (x, y, z, w) => x }._1 - 1,
    grid.keySet.maxBy { case (x, y, z, w) => x }._1 + 1
  ).inclusive
  val yRange = Range(
    grid.keySet.minBy { case (x, y, z, w) => y }._2 - 1,
    grid.keySet.maxBy { case (x, y, z, w) => y }._2 + 1
  ).inclusive
  val zRange = Range(
    grid.keySet.minBy { case (x, y, z, w) => z }._3 - 1,
    grid.keySet.maxBy { case (x, y, z, w) => z }._3 + 1
  ).inclusive
  val wRange = Range(
    grid.keySet.minBy { case (x, y, z, w) => w }._4 - 1,
    grid.keySet.maxBy { case (x, y, z, w) => w }._4 + 1
  ).inclusive

  def activeCount : Int = grid.values.count(identity)

  def step() : TimeCubeGrid = {
    var newGrid = grid;

    for(x <- xRange) {
      for(y <- yRange) {
        for(z <- zRange) {
          for(w <- wRange) {
          val pos = (x, y, z, w)
            val current = grid.getOrElse(pos, false)
            val neighbors = activeNeighbors(pos)
            val next = (current, neighbors) match {
              case (true, 2) | (true, 3) | (false, 3) => true
              case _ => false
            }
            if (next) {
              newGrid += (pos -> true)
            } else {
              newGrid -= pos
            }
          }
        }
      }
    }

    TimeCubeGrid(newGrid)
  }

  def activeNeighbors(pos: (Int, Int, Int, Int)) : Int =  {
    var count = 0;
    for (x <- (-1 to 1)) {
      for (y <- (-1 to 1)) {
        for (z <- (-1 to 1)) {
          for (w <- (-1 to 1)) {
            if (x != 0 || y != 0 || z != 0 || w !=0) {
              val neighbor = (pos._1 + x, pos._2 + y, pos._3 + z, pos._4 + w);
              if (grid.getOrElse(neighbor, false)) {
                count += 1
              }
            }
          }
        }
      }
    }
    count
  }
}

object TimeCubeGrid {
  def parse(input: Iterator[String]) : TimeCubeGrid  = {
    val set = input
      .zipWithIndex
      .flatMap { case (line, y) =>
        line
          .zipWithIndex
          .filter { case (ch, x) => ch == '#' }
          .map { case (_, x) => ((x, y, 0, 0) -> true) }
      }.toMap

    TimeCubeGrid(set)
  }
}
