package parser

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

class FormulaParser extends RegexParsers with PackratParsers {
  def id: Parser[Id] = "[а-яА-Яa-zA-Z][а-яА-Яa-zA-Z0-9_]*".r ^^ Id

  def number: Parser[Number] = "-" ~> number ^^ (n => Number(-n.value)) |
    ("[0-9]+\\.[0-9]*".r | "[0-9]+".r) ^^ (n => Number(n.toDouble))

  def funcCall: Parser[FuncCall] = id ~ ("(" ~> expression <~ ")") ^^ { case id ~ ex => FuncCall(id, ex) }

  def value: Parser[Expression] = number | funcCall | id | ("(" ~> expression <~ ")")

  lazy val term: PackratParser[Expression] = (term ~ ("*" | "/") ~ value ^^ binOperation) | value
  lazy val expression: PackratParser[Expression] = (expression ~ ("+" | "-") ~ term ^^ binOperation) | term

  private def binOperation(p: Expression ~ String ~ Expression) = p match {
    case e1 ~ op ~ e2 => BinOperation(e1, BinOperator(op), e2)
  }
}
