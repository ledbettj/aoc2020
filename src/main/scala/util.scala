package util

import scala.io.Source

trait WithInput {
  val DayNumber : Int

  lazy val Input = Source
    .fromFile(String.format("./inputs/day%d", DayNumber))
    .getLines
}
