package expression

import value.{Environment, Value}

/**
  * Created by Dilyar on 4/20/17.
  */
trait Expression {
  def execute(env: Environment): Value
}
