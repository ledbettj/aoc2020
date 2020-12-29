package day24

import scala.collection.mutable.ListBuffer

// use coordinate system from https://stackoverflow.com/questions/2459402
trait Step {
  def step(pos: (Int, Int, Int)) : (Int, Int, Int)
}

object Step {
  def all() = List(East(), Southeast(), Southwest(), West(), Northeast(), Northwest())

  def apply(s: String) = {
    s match {
      case "e" => East()
      case "se" => Southeast()
      case "sw" => Southwest()
      case "w" => West()
      case "ne" => Northeast()
      case "nw" => Northwest()
      case t => throw new Exception(s"Unknown step: $t")
    }
  }

  def parse(input: String) : Vector[Step] = {
    var result = ListBuffer[Step]()
    val iter = input.toIterator

    while(iter.hasNext) {
      val ch = iter.next()

      result += (ch match {
        case 'e' | 'w' => Step(ch.toString())
        case _ => Step(s"${ch}${iter.next()}")
      })
    }

    result.toVector
  }
}

case class East() extends Step {
  def step(rgb: (Int, Int, Int)) = (rgb._1 + 1, rgb._2 - 1, rgb._3)
}
case class Southeast() extends Step {
  def step(rgb: (Int, Int, Int)) = (rgb._1 + 1, rgb._2, rgb._3 - 1)
}
case class Southwest() extends Step {
  def step(rgb: (Int, Int, Int)) = (rgb._1, rgb._2 + 1, rgb._3 - 1)
}
case class West() extends Step {
  def step(rgb: (Int, Int, Int)) = (rgb._1 - 1, rgb._2 + 1, rgb._3)
}
case class Northeast() extends Step {
  def step(rgb: (Int, Int, Int)) = (rgb._1, rgb._2 - 1, rgb._3 + 1)
}
case class Northwest() extends Step {
  def step(rgb: (Int, Int, Int)) = (rgb._1 - 1, rgb._2, rgb._3 + 1)
}

class TileWalker {
  var tiles = Set[(Int, Int, Int)]()

  def blackTiles = tiles.size

  def flip(instr: String) = {
    val pos = walk(instr)

    if (tiles.contains(pos)) {
      tiles -= pos
    } else {
      tiles += pos
    }
  }

  def walk(instr: String) : (Int, Int, Int) = {
    val path = Step.parse(instr)
    val start = (0, 0, 0)

    path.foldLeft(start) { case (pos, step) => step.step(pos) }
  }
}

object TileWalker {
  def neighbors(pos: (Int, Int, Int)) = Step.all().map(step => step.step(pos)).toSet

  def tileArtDay(tiles: Set[(Int, Int, Int)]) : Set[(Int, Int, Int)] = {
    var result = Set[(Int, Int, Int)]()
    var toExamine = tiles

    tiles.foreach(t => toExamine = toExamine ++ neighbors(t))

    toExamine.foreach(tile => {
      val isBlack = tiles.contains(tile)
      val blackNeighbors = neighbors(tile).count(tiles.contains _)
      if (isBlack && (blackNeighbors == 0 || blackNeighbors > 2)) {
        // black to white, do not add.
      } else if (!isBlack && blackNeighbors == 2) {
        // white to black, add
        result += tile
      } else if (isBlack) {
        // maintain existing state
        result += tile
      }
    })

    result
  }
}
