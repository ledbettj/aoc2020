package day18

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Spec extends AnyFlatSpec with Matchers {

  "Part1" should "work for samples" in {
    Part1.solveOne("1 + 2 * 3") shouldEqual (1 + 2) * 3
    Part1.solveOne("1 + (2 * 3)") shouldEqual 1 + (2 * 3)
    Part1.solveOne("(1 + (2 * 3)) + (4 * (5 + 6))") shouldEqual 51
  }

  "Part1 solution" should "do shit" in {
    Part1.solve(InputLines.forDay(18)) shouldEqual 18213007238947L
  }
}
