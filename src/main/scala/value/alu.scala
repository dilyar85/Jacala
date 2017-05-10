package value

import expression.Identifier
import system.{TypeException, UndefinedException}

/**
  * Created by Dilyar on 4/25/17.
  */
object alu {
  def execute(operator: Identifier, args: List[Value]): Value = {
    operator.name match {
      case "add" => add(args)
      case "sub" => sub(args)
      case "mul" => mul(args)
      case "div" => div(args)
      case "less" => less(args)
      case "more" => more(args)
      case "equals" => equals(args)
      case "unequals" => unequals(args)
      case _ => throw new UndefinedException(operator)
    }
  }

  private def add(args: List[Value]): Number = {
    if (args.isEmpty) throw new TypeException("Addition input cannot be empty!")
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length) throw new TypeException("Inputs to add must be numbers!")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ + _)
  }

  private def sub(args: List[Value]): Value = {
    if (args.isEmpty) throw new TypeException("Subtraction input cannot be empty!")
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length) throw new TypeException("Inputs to subtract must be numbers!")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ - _)
  }

  private def mul(args: List[Value]): Value = {
    if (args.isEmpty) throw new TypeException("Multiplication input cannot be empty!")
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length) throw new TypeException("Inputs to multiply must be numbers!")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ * _)
  }

  private def div(args: List[Value]): Value = {
    if (args.isEmpty) throw new TypeException("Divisors cannot be empty!")
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length) throw new TypeException("Inputs to divide must be numbers!")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ / _)
  }

  private def less(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("Inequality must have two inputs!")
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length < args.length) throw new TypeException("Inputs to be compared must be numbers!")
    val nums2 = args.map(_.asInstanceOf[Number])
    nums2.head < nums2(1)
  }

  private def more(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("Inequality must have two inputs!")
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length < args.length) throw new TypeException("Inputs to be compared must be numbers!")
    val nums2 = args.map(_.asInstanceOf[Number])
    nums2.head > nums2(1)
  }

  private def equals(args: List[Value]): Value = {
    if (args.size != 2)
      throw new TypeException("Equality must have two inputs!")
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length < args.length) throw new TypeException("Inputs to be compared must be numbers!")
    val args2 = args.map(_.asInstanceOf[Number])
    args2.head == args2(1)
  }

  private def unequals(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("Unequals must have two numbers!")
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length < args.length) throw new TypeException("Inputs to be compared must be numbers!")
    val nums2 = args.map(_.asInstanceOf[Number])
    nums2.head != nums2(1)
  }

  private def not(args: List[Value]): Value = {
    if (args.length != 1) throw new TypeException("Not(!) operation must have only one input")
    val booles = args.filter(_.isInstanceOf[Boole])
    if (booles.length < args.length) throw new TypeException("Input must be Boolean!")
    val booles2 = args.map(_.asInstanceOf[Boole])
    booles2.head ! ()
  }

}
