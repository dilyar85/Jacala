package expression
import value.{Closure, Environment, Value}

/**
  * Created by Dilyar on 5/2/17.
  */
case class Lambda(params: List[Identifier], body: Expression) extends SpecialForm{
  override def execute(env: Environment): Value = new Closure(params, body, env)
}
