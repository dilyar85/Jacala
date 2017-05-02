package expression

import ui.TypeException
import value.{Boole, Environment}

/**
  * Created by Dilyar on 4/25/17.
  */
case class Disjunction(val exps: List[Expression]) extends SpecialForm {

  def execute(env: Environment) = {
    var more = true
    var result = Boole(false)
    for(exp <- exps if more) {
      val arg = exp.execute(env)
      if(!arg.isInstanceOf[Boole]) throw  new TypeException(arg + " must be type of Boole")
      val b = arg.asInstanceOf[Boole]
      if(b.value) {
        result = Boole(true)
        more = false
      }
    }
    result
  }

}
