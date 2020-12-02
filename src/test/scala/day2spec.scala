package day2

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {
  "Part1" should "implement working password rule passing" in {
    val input = List("1-4 d: abcfedd").toIterator
    val day2 = new Day2(input)

    day2.Entries.size shouldEqual 1
    day2.Entries(0).rule shouldEqual PasswordRule('d', 1 to 4)
    day2.Entries(0).password shouldEqual "abcfedd"
  }

  "Part1" should "implement working password validating" in {
    val rule = PasswordRule('c', 1 to 3)
    rule.isValid("c") shouldEqual true
    rule.isValid("cc") shouldEqual true
    rule.isValid("ccc") shouldEqual true
    rule.isValid("cccc") shouldEqual false
  }

  "Part1" should "calculate solution" in {
    val day2 = new Day2(InputLines.forDay(2))
    val validCount = day2.Entries.filter(_.isValid).size

    validCount shouldEqual 456
  }

  "Part2" should "implement working password validating" in {
    val rule = PasswordRule('c', 1 to 3)
    rule.isValidPart2("cde") shouldEqual true
    rule.isValidPart2("cdc") shouldEqual false
    rule.isValidPart2("edc") shouldEqual true
    rule.isValidPart2("ede") shouldEqual false
  }

  "Part2" should "calculate solution" in {
    val day2 = new Day2(InputLines.forDay(2))
    val validCount = day2.Entries.filter(_.isValidPart2).size

    validCount shouldEqual 308
  }
}
