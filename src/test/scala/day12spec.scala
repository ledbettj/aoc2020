package day12

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List("F10", "N3", "F7", "R90", "F11")

  "Part1" should "work for sample" in {
    val instructions = sampleInput.map(line => Instruction(line))
    val ship = Part1Navigator.navigate(
      Ship(Direction.East, (0, 0)),
      instructions.toIterator
    )

    ship.manhattan() shouldEqual 25
  }

  "Part1" should "solve solution" in {
    val instructions = InputLines.forDay(12).map(line => Instruction(line))
    val ship = Part1Navigator.navigate(
      Ship(Direction.East, (0, 0)),
      instructions.toIterator
    )

    ship.manhattan() shouldEqual 2847

  }

  "Part2" should "solve sample" in {
    var state = (Ship(Direction.East, (0, 0)), Ship(Direction.East, (10, 1)))
    val instructions = sampleInput.map(line => Instruction(line))

    instructions.foreach(i => {
      state = i.runWp(state._1, state._2)
    })
    (state._1.pos._1.abs + state._1.pos._2.abs) shouldEqual 286
  }

  "Part2" should "solve solution" in {
    var state = (Ship(Direction.East, (0, 0)), Ship(Direction.East, (10, 1)))
    val instructions = InputLines.forDay(12).map(line => Instruction(line))

    instructions.foreach(i => state = i.runWp(state._1, state._2))
    (state._1.pos._1.abs + state._1.pos._2.abs) shouldEqual 29839
  }
}
