package value

import expression.Identifier
import system.UndefinedException

import scala.collection.mutable

/**
  * Created by Dilyar on 4/20/17.
  */
class Environment(var extension: Environment = null) extends mutable.HashMap[Identifier, Value] with Value {


  override def apply(name: Identifier): Value = {
    if (this.contains(name)) super.apply(name)
    else if (this.extension != null) this.extension.apply(name)
    else throw new UndefinedException(name)
  }
}
