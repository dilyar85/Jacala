package expression

import ui.TypeException
import value.{Boole, Environment, Notification}

/**
  * Created by Dilyar on 4/30/17.
  */
case class Conditional(condition: Expression, consequence: Expression, alternative: Expression = null) extends SpecialForm {
  def execute(env: Environment) = {
    condition.execute(env) match {
      case Boole(value) =>
        if (value)
          consequence.execute(env)
        else if (alternative != null)
          alternative.execute(env)
        else
          Notification.FAIL
      case _ => throw new TypeException("Condition must be Boole!")
    }
  }
}
