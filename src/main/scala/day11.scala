package day11

import scala.collection.mutable.Map

trait Seat {
  val c: Char
}

case class Floor() extends Seat {
  val c = '.'
}

case class Occupied() extends Seat {
  val c = '#'
}

case class Empty() extends Seat {
  val c = 'L'
}

case class SeatLayout(val grid: Map[(Int, Int), Seat]) {

  def step() : Option[SeatLayout] = {
    var next = Map[(Int, Int), Seat]();
    var dirty = false

    grid.foreach { case ((x0, y0), seat) =>
      val neighbors = List(
        (-1, -1), (-1, 0), (0, -1),
        (1, 0), (1, 1), (0, 1),
        (-1, 1), (1, -1)
      )

      val occupiedNeighbors = neighbors
        .count{ case (x, y) => grid.getOrElse((x0 + x, y0 + y), Floor()) == Occupied()  }

      val s = seat match {
        case Empty() if occupiedNeighbors == 0 => { dirty = true; Occupied() }
        case Occupied() if occupiedNeighbors >= 4 => { dirty = true; Empty() }
        case _ => seat
      }

      next((x0, y0)) = s
    }

    if (!dirty)
      return None

    Some(SeatLayout(next))
  }

  def step2() : Option[SeatLayout] = {
    var next = Map[(Int, Int), Seat]();
    var dirty = false
    var w = grid.keySet.maxBy { case (x, y) => x }._1
    var h = grid.keySet.maxBy { case (x, y) => y }._2
    println(s"w is $w h is $h")
    grid.foreach { case ((x0, y0), seat) =>
      val directions = List(
        (-1, -1), (-1, 0), (0, -1),
        (1, 0), (1, 1), (0, 1),
        (-1, 1), (1, -1)
      )

      var seen = 0
      directions.foreach { case (dx, dy) =>
        var doBreak = false
        var pos = (x0 + dx, y0 + dy)

        while (!doBreak && pos._1 >= 0 && pos._1 <= w && pos._2 >=0 && pos._2 <= h) {
          grid.getOrElse(pos, Floor()) match {
            case Occupied() =>  seen += 1; doBreak = true
            case Empty() => doBreak = true
            case _ => doBreak = false
          };
          pos = (pos._1 + dx, pos._2 + dy)
        }
      }

      val s = seat match {
        case Empty() if seen == 0 => { dirty = true; Occupied() }
        case Occupied() if seen >= 5 => { dirty = true; Empty() }
        case _ => seat
      }

      next((x0, y0)) = s
    }

    if (!dirty)
      return None

    Some(SeatLayout(next))
  }


  def display() {
    (0 until 10).foreach(y => {
      (0 until 10).foreach(x => {
        print(s"${grid((x, y)).c}")
      })
      println("")
    })
    println()
  }

  def occupiedCount() = {
    grid.values.count { case Occupied() => true; case _ => false }
  }
}


object SeatLayout {
  def parse(input: Iterator[String]) : SeatLayout = {
    var grid = Map[(Int, Int), Seat]();

    input
      .zipWithIndex
      .foreach { case (line, y) =>
        line
          .zipWithIndex
          .foreach { case (ch, x) =>
            grid((x, y)) = ch match {
              case '.' => Floor()
              case '#' => Occupied()
              case 'L' => Empty()
            }
        }
      }

    SeatLayout(grid)
  }

}

