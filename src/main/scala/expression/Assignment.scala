package expression
import system.TypeException
import value.{Environment, Notification, Value, Variable}

/**
  * Created by Dilyar on 5/16/17.
  */
case class Assignment(id: Identifier, exp: Expression) extends SpecialForm{
  override def execute(env: Environment): Value = {
    val res = id.execute(env)
    res match {
      case v: Variable =>
        v.content = exp.execute(env)
        Notification.DONE
      case _ => throw new TypeException("Type error. Need variable here")
    }
  }
}
