package value

import expression.Literal

/**
  * Created by Dilyar on 4/20/17.
  */
case class Boole(value: Boolean) extends Literal {
  def &&(other: Boole) = Boole(this.value && other.value)

  def ||(other: Boole) = Boole(this.value || other.value)

  def !() = Boole(!value)


  override def toString = value.toString
}
