package util

import scala.io.Source

object InputLines {
  def forDay(day: Int) : Iterator[String] = Source.fromFile(s"./inputs/day$day").getLines
}
