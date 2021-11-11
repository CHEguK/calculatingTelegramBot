package parser

import scala.util.parsing.input.CharSequenceReader

object RunParser extends FormulaParser {
  def apply(code: String): Either[ParserError, Expression] =
    parse(expression, new PackratReader(new CharSequenceReader(code))) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(ParserError(msg))
    }

  case class ParserError(msg: String)
}
