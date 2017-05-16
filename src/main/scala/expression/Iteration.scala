package expression
import system.TypeException
import value.{Boole, Environment, Notification, Value}

/**
  * Created by Dilyar on 5/16/17.
  */
case class Iteration(condition: Expression, exp: Expression) extends SpecialForm{
  override def execute(env: Environment): Value = {
    var result: Value = Notification.UNKNOWN
    val con = condition.execute(env)
    con match {
      case boole: Boole =>
        while (boole.value) result = exp.execute(env)
        result
      case _ => throw new TypeException("Type exception, required boole in iteration")
    }

  }
}
