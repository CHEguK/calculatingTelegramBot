package parser

object Evaluator {
  def apply(expression: Expression,
            variables: Map[String, Double] = Map.empty,
            functions: Map[String, Either[String, Double] => Either[String, Double]] = Map.empty): Either[String, Double] = {
    def eval(exp: Expression): Either[String, Double] = this (exp, variables, functions)

    expression match {
      case Number(value) => Right(value)
      case Id(name) => if (variables.contains(name)) {
        Right(variables(name))
      } else {
        Left(s"\'$name\' - Неизвестное значение")
      }
      case BinOperation(left, op, right) => operator2func(op)(eval(left), eval(right))
      case FuncCall(funcId, expr) => if (functions.contains(funcId.name)) {
        functions(funcId.name)(eval(expr))
      } else {
        Left(s"\'${funcId.name}\' - Неизвестная функция")
      }
    }
  }

  def operator2func(binOperator: BinOperator)(left: Either[String, Double], right: Either[String, Double]): Either[String, Double] = {
    (left, right) match {
      case (Right(a), Right(b)) => binOperator.op match {
        case "+" => Right(a + b)
        case "-" => Right(a - b)
        case "*" => Right(a * b)
        case "/" =>
          if (b > 0) {
            Right(a / b)
          } else {
            Left("На ноль делить нельзя, сука") // TODO: "суку" лучше убрать
          }
      }
      case (Left(msg), _) => Left(msg)
      case (_, Left(msg)) => Left(msg)
    }
  }
}
