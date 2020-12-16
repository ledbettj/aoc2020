package day15

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List[Long](0,3,6)

  "Part1" should "work for sample input" in {
    val g = new NumberGame(sampleInput)
    while (g.turn != 11) {
      g.step()
    }
    g.current shouldEqual 0
  }

  "Part1" should "work for real input" in {
    val g = new NumberGame(List[Long](0, 20, 7, 16, 1, 18, 15))
    while (g.turn != 2021) {
      g.step()
    }
    g.current shouldEqual 1025
  }

  "Part2" should "work for real input" in {
    val g = new NumberGame(List[Long](0, 20, 7, 16, 1, 18, 15))
    while (g.turn != 30_000_001) {
      g.step()
    }
    g.current shouldEqual 129262
  }
}
