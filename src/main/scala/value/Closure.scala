package value

import expression.{Expression, Identifier}
import system.TypeException


//Closure has params, body and defEnv(it kind of organizes lambda)
case class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value{
  def apply(args: List[Value]): Value = {
    //Check params and args length
    if(params.length != args.length)
      throw new TypeException("parameters and arguments must be same length!")
    //1.create localEnv extending defEnv
    val localEnv = new Environment(defEnv)
    //2.bind params to args in localEnv
    for((param, arg) <- params zip args) localEnv.put(param, arg)
    //3.body.execute(localEnv)
    body.execute(localEnv)
  }

  override def toString = "Closure(" + params.toString + body.toString + ")"
}
