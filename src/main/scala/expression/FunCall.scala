package expression

import value.{Environment, Value, alu}

/**
  * Created by Dilyar on 4/25/17.
  */
case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {

  def execute(env: Environment): Value = {

    val args = operands.map(_.execute(env))
    alu.execute(operator, args)
  }

}
