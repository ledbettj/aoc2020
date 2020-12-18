package day18
import scala.collection.mutable.Stack

sealed trait Token;

case class Literal(value: Long) extends Token;
case class Mult() extends Token;
case class Add() extends Token;
case class LParen() extends Token;
case class RParen() extends Token;

sealed trait Operator {
  def eval(lhs: Long, rhs: Long) : Long
}

case class OpAdd() extends Operator {
  def eval(lhs: Long, rhs: Long)  = lhs + rhs
}
case class OpMult() extends Operator {
  def eval(lhs: Long, rhs: Long)  = lhs * rhs
}

object Token {
  def apply(ch: Char) : Token = {
    ch match {
      case '*' => Mult()
      case '+' => Add()
      case '(' => LParen()
      case ')' => RParen()
      case c if c.isDigit => Literal(c.toString().toLong)
      case _ => throw new Exception("Oh Well")
    }
  }

  def tokenize(input: String) : List[Token] = {
    input
      .filter(!_.isWhitespace)
      .map(ch => Token(ch))
      .toList
  }
}

object Part1 {
  def solve(input: Iterator[String]) : Long = {
    input
      .map(solveOne _)
      .sum
  }

  def solveOne(line: String) : Long = {
    evaluate(Token.tokenize(line).toIterator)
  }

  def evaluate(tokens: Iterator[Token]) : Long = {
    var rhs = 0L;
    var lhs = tokens.next() match {
      case Literal(v) => v
      case LParen() => evaluate(tokens)
      case t => throw new Exception(s"Parse error, unexpected token $t: expected ( or literal")
    }
    var op : Operator = OpAdd()

    while(tokens.hasNext) {
      op = tokens.next() match {
        case RParen() => return lhs
        case Add() => OpAdd()
        case Mult() => OpMult()
        case t => throw new Exception(s"Parse error, unexpected token $t: expected ) or operator")
      }
      rhs = tokens.next() match {
        case Literal(v) => v
        case LParen() => evaluate(tokens)
        case t => throw new Exception(s"Parse error, unexpected token $t: expected ( or literal")
      }
      println(s"$lhs $op $rhs")
      lhs = op.eval(lhs, rhs)
    }

    lhs
  }
}


class Node(var value: Long, var children: IndexedSeq[Node] = IndexedSeq());

object Parser {
  def parse(line: String) : Node = {
    val tokens = Token.tokenize(line).toIterator

    parseExpression(tokens)
  }

  def parseExpression(tokens: Iterator[Token]) : Node = {
    val lhs = parseTerm(tokens)

  }

  def parseBinary(lhs: Node, 

  def parseTerm(tokens: Iterator[Token]) : Node = {
    tokens.next() match {
      case LParen() => parseExpression(tokens)
      case Literal(v) => new Node(v)
    }
  }

}
