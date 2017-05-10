package expression

import value._
import system._

/**
  * Created by Dilyar on 4/30/17.
  */
case class Conjunction(val exps: List[Expression]) extends SpecialForm {

  def execute(env: Environment) = {
    var more = true
    var result = Boole(true)
    for (exp <- exps if more) {
      val arg = exp.execute(env)
      if (!arg.isInstanceOf[Boole]) throw new TypeException(arg.toString + " must be type of Boole")
      val b = arg.asInstanceOf[Boole]
      if (!b.value) {
        result = Boole(false)
        more = false
      }
    }
    result
  }

}
