package day4

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
    "byr:1937 iyr:2017 cid:147 hgt:183cm",
    "",
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
    "hcl:#cfa07d byr:1929",
    "",
    "hcl:#ae17e1 iyr:2013",
    "eyr:2024",
    "ecl:brn pid:760753108 byr:1931",
    "hgt:179cm",
    "",
    "hcl:#cfa07d eyr:2025 pid:166559648",
    "iyr:2011 ecl:brn hgt:59in"
  )

  "Day4" should "read passports correctly" in {
    val day4 = new Day4(sampleInput.toIterator)
    day4.passports.size shouldEqual 4
    // inserted in reverse order, so this is the first input above
    day4.passports(3).keys shouldEqual Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "cid", "hgt")
    day4.passports(3).isLooslyValid shouldEqual true
  }

  "Part1" should "calculate solution" in {
    val day4 = new Day4(InputLines.forDay(4))
    day4.passports.count(_.isLooslyValid) shouldEqual 222
  }

  "Part2" should "work for sample data" in {
    val invalid = List(
      "eyr:1972 cid:100",
      "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
      "",
      "iyr:2019",
      "hcl:#602927 eyr:1967 hgt:170cm",
      "ecl:grn pid:012533040 byr:1946",
      "",
      "hcl:dab227 iyr:2012",
      "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
      "",
      "hgt:59cm ecl:zzz",
      "eyr:2038 hcl:74454a iyr:2023",
      "pid:3556412378 byr:2007"
    )

    val valid = List(
      "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
      "hcl:#623a2f",
      "",
      "eyr:2029 ecl:blu cid:129 byr:1989",
      "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
      "",
      "hcl:#888785",
      "hgt:164cm byr:2001 iyr:2015 cid:88",
      "pid:545766238 ecl:hzl",
      "eyr:2022",
      "",
      "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
    )

    var day4 = new Day4(invalid.toIterator)
    day4.passports.count(_.isStrictlyValid) shouldEqual 0

    day4 = new Day4(valid.toIterator)
    day4.passports.count(_.isStrictlyValid) shouldEqual 4
  }

  "Part2" should "calculate solution" in {
    var day4 = new Day4(InputLines.forDay(4))
    day4.passports.count(_.isStrictlyValid) shouldEqual 141
  }
}
