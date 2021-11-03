package parser

sealed trait Expression

case class BinOperator(op: String)

case class Number(value: Double) extends Expression
case class Id(name: String) extends Expression
case class BinOperation(left: Expression, op: BinOperator, right: Expression) extends Expression
case class FuncCall(funcCall: Id, argument: Expression) extends Expression
