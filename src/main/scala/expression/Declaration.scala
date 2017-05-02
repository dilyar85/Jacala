package expression

import value.{Environment, Notification, Value}

/**
  * Created by Dilyar on 4/25/17.
  */
case class Declaration(val name: Identifier, val body: Expression) extends SpecialForm {

  def execute(env: Environment): Value = {
    val result = body.execute(env)
    env.put(name, result)
    Notification.OK
  }


}
