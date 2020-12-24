package day22

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day22Spec extends AnyFlatSpec with Matchers {
  val sample = List(
    "Player 1:",
    "9",
    "2",
    "6",
    "3",
    "1",
    "",
    "Player 2:",
    "5",
    "8",
    "4",
    "7",
    "10"
  )

  "Part1" should "work for sample" in {
    var c = Combat.parse(sample)

    while (!c.isGameOver()) {
      c = c.turn()
    }

    c.winnerScore shouldEqual 306
  }

  "Part1" should "work for answer" in {
    var c = Combat.parse(InputLines.forDay(22).toList)

    while (!c.isGameOver()) {
       c = c.turn()
    }

    c.winnerScore shouldEqual 35005
  }

}
