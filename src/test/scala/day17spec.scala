package day17

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    ".#.",
    "..#",
    "###"
  )

  "Part1" should "work for sample" in {
    var g = CubeGrid.parse(sampleInput.toIterator)
    g.activeCount shouldEqual 5
    g = g.step
    g.activeCount shouldEqual 11
    g = g.step().step().step().step().step()
    g.activeCount shouldEqual 112
  }

  "Part1" should "work for input" in {
    var g = CubeGrid.parse(InputLines.forDay(17))
    g = g.step().step().step().step().step().step()
    g.activeCount shouldEqual 295
  }

  "Part2" should "idk copy paste work" in {
    var g = TimeCubeGrid.parse(InputLines.forDay(17))
    g = g.step().step().step().step().step().step()
    g.activeCount shouldEqual 1972

  }

}
