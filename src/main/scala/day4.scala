package day4

import scala.util.matching.Regex

trait Validator {
  def isValid(value: String) : Boolean
}

class YearValidator(range: Range) extends Validator {
  def isValid(value: String): Boolean = value.toIntOption.map(range.contains(_)).getOrElse(false)
}

class RegexValidator(regex: Regex) extends Validator {
  def isValid(value: String): Boolean = regex.matches(value)
}

class HexColorValidator extends RegexValidator(raw"^#[0-9A-Fa-f]{6}$$".r)

class SetValidator(set: Set[String]) extends Validator {
  def isValid(value: String): Boolean = set.contains(value)
}

object SetValidator {
  def apply(s: String*): SetValidator = new SetValidator(Set(s:_*))
}

class HeightValidator(unitRanges: Map[String, Range]) extends Validator {
  val regex = raw"^(\d+)(\w+)$$".r

  def isValid(value: String): Boolean = {
    regex
      .findFirstMatchIn(value)
      .map(matchData => {
        val size = matchData.group(1).toInt
        val unit = matchData.group(2).toString.toLowerCase
        unitRanges
          .get(unit)
          .map(_.contains(size))
          .getOrElse(false)
      })
      .getOrElse(false)
  }
}

class Passport {
  var data: Map[String, String] = Map()

  val RequiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val Validators : Map[String, Validator] = Map(
    "byr" -> new YearValidator(1920 to 2002),
    "iyr" -> new YearValidator(2010 to 2020),
    "eyr" -> new YearValidator(2020 to 2030),
    "hgt" -> new HeightValidator(Map("in" -> (59 to 76), "cm" -> (150 to 193))),
    "hcl" -> new HexColorValidator(),
    "ecl" -> SetValidator("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    "pid" -> new RegexValidator(raw"^[0-9]{9}$$".r)
  )

  // part1
  def isLooslyValid : Boolean = RequiredFields.subsetOf(data.keySet)

  // part2
  def isStrictlyValid : Boolean = {
    isLooslyValid && Validators.forall { case (fieldName, validator) =>
      validator.isValid(data(fieldName))
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
  def insert(kv : (String, String)) = data = data + kv
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
