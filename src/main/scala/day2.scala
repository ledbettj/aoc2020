package day2

import util.InputLines
import scala.collection.immutable.Range

case class PasswordRule(char: Char, range: Range) {
  def isValid(password: String) : Boolean = {
    val count = password.chars().filter(_ == char).count

    range.contains(count)
  }

  def isValidPart2(password: String) : Boolean = {
    charIsMatch(password, range.start) ^ charIsMatch(password, range.end)
  }

  private

  def charIsMatch(password: String, index: Int) : Boolean = {
    index - 1 < password.size && (password.charAt(index - 1) == char)
  }
}

class PasswordEntry(val rule: PasswordRule, val password: String) {
  def isValid : Boolean = rule.isValid(password)
  def isValidPart2 : Boolean = rule.isValidPart2(password)
}

class Day2(input: Iterator[String]) {
  lazy val Pattern = raw"(\d+)-(\d+)\s+(\w):\s+(\w+)".r
  lazy val Entries = input
    .map(line => {
      Pattern
        .findFirstMatchIn(line)
        .map(m => {
          val rule = PasswordRule(
            m.group(3).toString.charAt(0),
            m.group(1).toString.toInt to m.group(2).toString.toInt
          )
          new PasswordEntry(rule, m.group(4).toString)
        })
        .get
    })
    .toVector
}
