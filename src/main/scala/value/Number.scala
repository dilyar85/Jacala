package value

import expression.Literal

/**
  */
case class Number(value: Double) extends Literal {
  def +(other: Number) = Number(this.value + other.value)
  def *(other: Number) = Number(this.value * other.value)
  def -(other: Number) = Number(this.value - other.value)
  def /(other: Number) = Number(this.value / other.value)
  def <(other: Number) = Boole(this.value < other.value)
  def >(other: Number) = Boole(this.value > other.value)
  def ==(other: Number) = Boole(this.value == other.value)
  def !=(other: Number) = Boole(this.value != other.value)
  override def toString  = value.toString
}