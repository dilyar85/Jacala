package expression

import value.{Environment, Value}

/**
  * Created by Dilyar on 5/2/17.
  */
case class Block(locals: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    //execute each one and return last one
    val localEnv = new Environment(env)
    val args = locals.map(_.execute(localEnv))
    args.last
  }
}
