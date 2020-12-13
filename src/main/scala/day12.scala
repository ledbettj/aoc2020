package day12

trait Instruction {
  val value : Int

  def run(ship: Ship) : Ship

  def runWp(ship: Ship, wp: Ship) : (Ship, Ship)
}

object Instruction {
  def apply(input: String) : Instruction = {
    val value = input.substring(1).toInt

    input.charAt(0) match {
      case 'N' => North(value)
      case 'S' => South(value)
      case 'E' => East(value)
      case 'W' => West(value)
      case 'L' => Left((value % 360) / 90) // convert degrees to # of turns
      case 'R' => Right((value % 360) / 90)
      case 'F' => Forward(value)
    }
  }
}

object Direction extends Enumeration {
  val North, South, East, West = Value;
}

case class Ship(heading: Direction.Value, pos: (Int, Int)) {
  def manhattan() = (pos._1.abs + pos._2.abs).toInt

  def move(dx: Int, dy: Int) = Ship(heading, (pos._1 + dx, pos._2 + dy))

  def rotateRight(turnCount: Int) : Ship = {
    var newHeading = (0 until turnCount)
      .foldLeft(heading)((h, _) =>
        h match {
          case Direction.North => Direction.East
          case Direction.East  => Direction.South
          case Direction.South => Direction.West
          case Direction.West  => Direction.North
        }
      )

    Ship(newHeading, pos)
  }

  def rotateLeft(turnCount: Int) : Ship = {
    rotateRight(4 - turnCount)
  }
}

case class North(val value : Int) extends Instruction {
  def run(ship: Ship) = ship.move(0, value)
  def runWp(ship: Ship, wp: Ship) = (ship, wp.move(0, value))
}
case class South(val value : Int) extends Instruction {
  def run(ship: Ship) = ship.move(0, -value)
  def runWp(ship: Ship, wp: Ship) = (ship, wp.move(0, -value))
}
case class East(val value : Int) extends Instruction {
  def run(ship: Ship) = ship.move(value, 0)
  def runWp(ship: Ship, wp: Ship) = (ship, wp.move(value, 0))
}
case class West(val value : Int) extends Instruction {
  def run(ship: Ship) = ship.move(-value, 0)
  def runWp(ship: Ship, wp: Ship) = (ship, wp.move(-value, 0))
}
case class Left(val value : Int) extends Instruction {
  def run(ship: Ship) = ship.rotateLeft(value)
  def runWp(ship: Ship, wp: Ship) = Right(4 - value).runWp(ship, wp)
}
case class Right(val value : Int) extends Instruction {
  def run(ship: Ship) = ship.rotateRight(value)
  def runWp(ship: Ship, wp: Ship) = {
    val pos = (0 until value).foldLeft(wp.pos)((pos, _) => (pos._2, -pos._1))

    (ship, Ship(wp.heading, pos))
  }
}
case class Forward(val value : Int) extends Instruction {
  def run(ship: Ship) : Ship = {
    ship.heading match {
      case Direction.North => North(value).run(ship)
      case Direction.South => South(value).run(ship)
      case Direction.East  => East(value).run(ship)
      case Direction.West  => West(value).run(ship)
    }
  }
  def runWp(ship: Ship, wp: Ship) = (ship.move(wp.pos._1 * value, wp.pos._2 * value), wp)
}

object Part1Navigator {
  def navigate(ship: Ship, instructions: Iterator[Instruction]) : Ship = {
    instructions.foldLeft(ship)((state, instr) => instr.run(state))
  }
}
