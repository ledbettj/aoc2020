package day4

import scala.collection.mutable

class Passport {
  var data: mutable.Map[String, String] = mutable.Map()
  // ignore cid for Part1
  val RequiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid") //, "cid")

  // part1
  def isLooslyValid : Boolean = RequiredFields.forall(data.keySet.contains(_))

  // part2
  def isStrictlyValid : Boolean = {
    isLooslyValid &&
    validYearBetween(data("byr"), 1920 to 2002) &&
    validYearBetween(data("iyr"), 2010 to 2020) &&
    validYearBetween(data("eyr"), 2020 to 2030) &&
    validHeight(data("hgt")) &&
    raw"^#[0-9a-fA-F]{6}$$".r.matches(data("hcl")) &&
    Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(data("ecl")) &&
    "^[0-9]{9}$".r.matches(data("pid"))
  }

  private def validYearBetween(value: String, range: Range) : Boolean = {
    value.toIntOption match {
      case Some(v) => range.contains(v)
      case None => false
    }
  }

  private def validHeight(value: String) : Boolean = {
    val regex = raw"^(\d+)(in|cm)$$".r

    regex.findFirstMatchIn(value) match {
      case None => false
      case Some(m) => {
        val size = m.group(1).toInt
        val unit = m.group(2).toString().toLowerCase()

        unit match {
          case "in" => (59 to 76).contains(size)
          case "cm" => (150 to 193).contains(size)
          case _ => false
        }
      }
    }
  }

  def appendLine(line: String) = {
    line
      .split(' ')
      .map(segment => {
        val result = segment.split(':')
        (result(0), result(1))
      })
    .foreach(insert)
  }

  def keys = data.keySet
  def insert(field : (String, String)) = data(field._1) = field._2
}

class Day4(input: Iterator[String]) {
  var passports : List[Passport] = input.foldLeft(List(new Passport()))((list, line) => {
    if (line.isBlank) {
      new Passport() :: list
    } else {
      list(0).appendLine(line)
      list
    }
  })
}
