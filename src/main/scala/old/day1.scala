package day1
import scala.io.Source

trait FuelCalculator {
  def fuelRequired(mass: Int) : Int = mass / 3 - 2
  def totalFuelRequired(masses: Iterator[Int] ) : Int = masses.map(fuelRequired).sum
  def recursiveTotalFuelRequired(masses: Iterator[Int]) : Int = masses.map(recursiveFuelRequired).sum

  def recursiveFuelRequired(mass: Int) : Int = {
    fuelRequired(mass) match {
      case value if value <= 0 => 0
      case value => value + recursiveFuelRequired(value)
    }
  }

}

trait WithInput {
  val DayNumber : Int

  lazy val Input = Source
    .fromFile(String.format("./inputs/old-day%d", DayNumber))
    .getLines
}

object Part1 extends FuelCalculator with WithInput {
  val DayNumber = 1
  lazy val Masses = Input.map(line => line.toInt)
 }

object Part2 extends FuelCalculator with WithInput {
  val DayNumber = 1
  lazy val Masses = Input.map(line => line.toInt)
 }
