package expression

import system.{TypeException, UndefinedException}
import value.{Closure, Environment, Value, alu}

/**
  * Created by Dilyar on 4/25/17.
  */
case class FunCall(operator: Expression, operands: List[Expression]) extends Expression {

  def execute(env: Environment): Value = {
    val args = operands.map(_.execute(env))
    try {
      val fun = operator.execute(env)
      if(!fun.isInstanceOf[Closure]) throw new TypeException("Operator must be type of Closure")
      fun.asInstanceOf[Closure].apply(args)
    } catch {
      case _: UndefinedException => alu.execute(operator.asInstanceOf[Identifier],args)
    }
  }

}
