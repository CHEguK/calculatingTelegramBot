import org.scalatest.funsuite.AnyFunSuite
import parser.{BinOperation, BinOperator, FormulaParser, FuncCall, Id, Number}

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader


class ParserTests extends AnyFunSuite {

  object PTest extends FormulaParser {
    def testNumber(terms: List[(String, Any)]) = {
      for {
        term <- terms
        parserResult = parse(number, term._1) match {
            case Success(result, _) => result.value
            case NoSuccess(_, _) => "Not Result"
          }
        _ = assert(parserResult === term._2)
      } ()
    }

    def testId(terms: List[(String, Any)]) = {
      for {
        term <- terms
        parserResult = parse(id, term._1) match {
          case Success(result, _) => result.name
          case NoSuccess(_, _) => "Not Result"
        }
        _ = assert(parserResult === term._2)
      } ()
    }

    def testFunCall(terms: List[(String, Any)]) = {
      for {
        term <- terms
        parserResult = parse(funcCall, new PackratReader(new CharSequenceReader(term._1))) match {
          case Success(result, _) => (result.funcCall.name, result.argument)
          case NoSuccess(_, _) => "Not Result"
        }
        _ = assert(parserResult === term._2)
      } ()
    }

    def testValue(terms: List[(String, Any)]) = {
      for {
        term <- terms
        parserResult = parse(value, new PackratReader(new CharSequenceReader(term._1))) match {
          case Success(result, _) => result
          case NoSuccess(_, _) => "Not Result"
        }
        _ = assert(parserResult === term._2)
      } ()
    }

    def testTerm(codes: List[(String, Any)]) = {
      for {
        code <- codes
        parserResult = parse(term, new PackratReader(new CharSequenceReader(code._1))) match {
          case Success(result, _) => result
          case NoSuccess(_, _) => "Not Result"
        }
        _ = assert(parserResult === code._2)
      } ()
    }

    def testExpression(codes: List[(String, Any)]) = {
      for {
        code <- codes
        parserResult = parse(expression, new PackratReader(new CharSequenceReader(code._1))) match {
          case Success(result, _) => result
          case NoSuccess(_, _) => "Not Result"
        }
        _ = assert(parserResult === code._2)
      } ()
    }
  }

  test("Test parser") {
    val dataForNumberTests = List(("123", 123.0), ("aaa", "Not Result"), ("123 Text", 123.0), ("123 Not Result 123", 123.0), ("1a2b3c", 1.0), ("1/2", 1.0), ("asv23", "Not Result"))
    val dataForIdTests = List(("sin", "sin"), ("cos1", "cos1"), ("pi", "pi"), ("син", "син"), ("син1", "син1"), ("1син", "Not Result"), ("123", "Not Result"), ("_", "Not Result"), ("exp_", "exp_"), ("_exp", "Not Result"))
    val dataForFunCallTests = List(("sin(1)", ("sin", Number(1))), ("син(1)", ("син", Number(1))), ("син(1 + 1)", ("син", BinOperation(Number(1.0), BinOperator("+"), Number(1.0)))), ("_(1)", "Not Result"),  ("123", "Not Result"), ("123()", "Not Result"))
    val dataForValueTests = List(("sin(1)", FuncCall(Id("sin"), Number(1.0))), ("син(1)", FuncCall(Id("син"), Number(1.0))), ("син(1 + 1)", FuncCall(Id("син"),BinOperation(Number(1.0),BinOperator("+"),Number(1.0)))), ("_(1)", "Not Result"),  ("123", Number(123.0)), ("123()", Number(123.0)), ("123 + sin(pi/2)", Number(123.0)), ("(pi/2) + 123 ", BinOperation(Id("pi"),BinOperator("/"),Number(2.0))))
    val dataForTermTests = List(("1 * 2", BinOperation(Number(1.0),BinOperator("*"),Number(2.0))), ("1 + 2", Number(1.0)), ("1 - 2", Number(1.0)), ("1 */ 2", Number(1.0)), ("*/dgr1*/2", "Not Result"))
    val dataForExpressionTests = List(
      ("sin(pi/2) +3/4 * 73.2- -12 + кос(1) + (21 - 1/2) +1 - син(пи) * (123 /1 + 13* 12)", BinOperation(BinOperation(BinOperation(BinOperation(BinOperation(BinOperation(FuncCall(Id("sin"),BinOperation(Id("pi"),BinOperator("/"),Number(2.0))),BinOperator("+"),BinOperation(BinOperation(Number(3.0),BinOperator("/"),Number(4.0)),BinOperator("*"),Number(73.2))),BinOperator("-"),Number(-12.0)),BinOperator("+"),FuncCall(Id("кос"),Number(1.0))),BinOperator("+"),BinOperation(Number(21.0),BinOperator("-"),BinOperation(Number(1.0),BinOperator("/"),Number(2.0)))),BinOperator("+"),Number(1.0)),BinOperator("-"),BinOperation(FuncCall(Id("син"),Id("пи")),BinOperator("*"),BinOperation(BinOperation(Number(123.0),BinOperator("/"),Number(1.0)),BinOperator("+"),BinOperation(Number(13.0),BinOperator("*"),Number(12.0)))))),
      ("facup sin(pi/2) / +3/4 * 73.2- -12 + кос(1) + (21 - 1/2) +1 - син(пи) * (123 /1 + 13* 12)", Id("facup"))
    )


    PTest.testNumber(dataForNumberTests)
    PTest.testId(dataForIdTests)
    PTest.testFunCall(dataForFunCallTests)
    PTest.testValue(dataForValueTests)
    PTest.testTerm(dataForValueTests ++ dataForTermTests)
    PTest.testExpression(dataForExpressionTests)
  }
}