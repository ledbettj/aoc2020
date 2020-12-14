package day14

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    "mem[8] = 11",
    "mem[7] = 101",
    "mem[8] = 0"
  )

  "Part1" should "work for sample input" in {
    val p = new Program(sampleInput.toIterator)
    val s = p.run()
    s.memorySum shouldEqual 165
  }

  "Part1" should "work for real input" in {
    val p = new Program(InputLines.forDay(14))
    val s = p.run()
    s.memorySum shouldEqual 17934269678453L
  }


  "Part2" should "work for sample input" in {
    val sample = List(
      "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
    )
    val p = new Program(sample.toIterator)
    val s = p.runV2()
    s.memorySum shouldEqual 208
  }


  "Part2" should "work for real input" in {
    val p = new Program(InputLines.forDay(14))
    val s = p.runV2()
    s.memorySum shouldEqual 3440662844064L
  }

}
