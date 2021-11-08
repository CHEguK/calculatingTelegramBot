package parser

object Evaluator {
  def apply(expression: Expression,
            variables: Map[String, Double] = Map.empty,
            functions: Map[String, (Double) => Double] = Map.empty): Double = {
    def eval(exp: Expression) = this (exp, variables, functions)

    expression match {
      case Number(value) => value
      case Id(name) => if (variables.contains(name)) {
        variables(name)
      } else {
        1.0
      }
      case BinOperation(left, op, right) => operator2func(op)(eval(left), eval(right))
      case FuncCall(funcId, expr) => if (functions.contains(funcId.name)) {
        functions(funcId.name)(eval(expr))
      } else {
        1.0
      }
    }
  }

  def operator2func(binOperator: BinOperator): (Double, Double) => Double =
    binOperator.op match {
      case "+" => (a, b) => a + b
      case "-" => (a, b) => a - b
      case "*" => (a, b) => a * b
      case "/" => (a, b) => {
        if (b > 0) {
          a / b
        } else {
          1.0
        }
      }
    }
}
