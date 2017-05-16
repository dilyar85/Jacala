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
      // primitive I/O ops:
      case "not" => not(args)
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      case _ => throw new UndefinedException(operator)
    }
  }

  private def castAsNumbers(vals: List[Value], opcode: String): List[Number] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException(opcode + " inputs must be numbers")
    vals.map(_.asInstanceOf[Number])
  }


  private def add(args: List[Value]): Number = {
    castAsNumbers(args, "add").reduce(_ + _)
  }

  private def sub(args: List[Value]): Value = {
    castAsNumbers(args, "sub").reduce(_ - _)
  }

  private def mul(args: List[Value]): Value = {
    castAsNumbers(args, "mul").reduce(_ * _)
  }

  private def div(args: List[Value]): Value = {
    castAsNumbers(args, "div").reduce(_ / _)
  }

  private def less(args: List[Value]): Value = {
    val nums = castAsNumbers(args, "less")
    if (args.length != 2) throw new TypeException("less inputs must be numbers")
    if (nums.head.value < nums(1).value) Boole(true) else Boole(false)
  }

  private def more(args: List[Value]): Value = {
    val nums = castAsNumbers(args, "more")
    if (args.length != 2) throw new TypeException("more inputs must be numbers")
    if (nums.head.value > nums(1).value) Boole(true) else Boole(false)
  }

  private def equals(args: List[Value]): Value = {
    if (args.isEmpty) throw new TypeException("equals expected > 0 inputs")
    var more = true
    var result = true
    for (i <- 1 until args.length if more)
      if (args(i) != args.head) {
        result = false
        more = false
      }
    Boole(result)
  }

  private def unequals(args: List[Value]): Value = {
    if (args.length != 2) throw new TypeException("unequals expected 2 inputs")
    if (args.head != args(1)) Boole(true) else Boole(false)
  }

  private def not(args: List[Value]): Value = {
    if (args.length != 1) throw new TypeException("not expected 1 input")
    if (!args.head.isInstanceOf[Boole]) throw new TypeException("input to not must be Boole")
    args.head.asInstanceOf[Boole] ! () // can't get Boole.! to work
  }

  def write(vals: List[Value]): Value = {
    println(vals.head); Notification.DONE
  }

  def read(vals: List[Value]): Value = {
    val result = readDouble(); Number(result)
  }

  def prompt(vals: List[Value]): Value = {
    print("=> "); Notification.DONE
  }


}
