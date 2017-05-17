package expression

import system.TypeException
import value.{Boole, Environment, Notification, Value}

/**
  * Created by Dilyar on 5/16/17.
  */
case class Iteration(condition: Expression, body: Expression) extends SpecialForm {

  override def execute(env: Environment): Value = {
    if (!condition.execute(env).isInstanceOf[Boole])
      throw new TypeException("Type exception. Must be boole to iterate.")

    while (condition.execute(env).asInstanceOf[Boole].value)
      body.execute(env)

    Notification.DONE
  }
}
