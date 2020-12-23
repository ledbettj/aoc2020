package day21

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day21Spec extends AnyFlatSpec with Matchers {
  val sample = List(
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
    "trh fvjkl sbzzf mxmxvkd (contains dairy)",
    "sqjhc fvjkl (contains soy)",
    "sqjhc mxmxvkd sbzzf (contains fish)"
  )

  "Part1" should "work for sample" in {
    val col = FoodCollection(sample.toIterator)
    col.notAllergens() shouldEqual Set("kfcds", "nhms", "sbzzf", "trh")
    col.notAllergenCount() shouldEqual 5
  }

  "Part1" should "work for input" in {
    val col = FoodCollection(InputLines.forDay(21))
    col.notAllergenCount() shouldEqual 2307
  }


  "Part2" should "work for sample" in {
    val col = FoodCollection(sample.toIterator)
    col.allergenMap() shouldEqual Map("mxmxvkd" -> "dairy", "sqjhc" -> "fish", "fvjkl" -> "soy")
    col.dangerousList shouldEqual "mxmxvkd,sqjhc,fvjkl"
  }

  "Part2" should "work for realz" in {
    val col = FoodCollection(InputLines.forDay(21))
    col.dangerousList shouldEqual "cljf,frtfg,vvfjj,qmrps,hvnkk,qnvx,cpxmpc,qsjszn"
  }

}
