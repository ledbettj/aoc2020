package day8

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import util.InputLines

class Day8Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"
  )

  "Part1" should "work for sample" in {
    val p = Program(sampleInput.toIterator)
    p.runUntilRepeat()
    p.state.accumulator shouldEqual 5
  }

  "Part1" should "solve p1" in {
    val p = Program(InputLines.forDay(8))
    p.runUntilRepeat()
    p.state.accumulator shouldEqual 1928
  }

  "Part2" should "solve p2" in {
    val set = ProgramSet(InputLines.forDay(8))

    val p = set.findTerminating().get

    p.state.accumulator shouldEqual 1319
  }
}
