package expression

import value.Environment

/**
  * Created by Dilyar on 4/20/17.
  */
case class Identifier(val name: String) extends Expression {

  def execute(env: Environment) = env(this)

}
