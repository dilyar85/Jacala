package expression

import value._

/**
  * Created by Dilyar on 4/20/17.
  */
trait Literal extends Expression with Value {

  def execute(env: Environment) = this

}
