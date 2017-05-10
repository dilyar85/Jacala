package system

import expression._

/**
  * Created by Dilyar on 5/2/17.
  */
class WookieParsers extends EwokParsers {


  override def term: Parser[Expression] = lambda | block | super.term



  override def funcall: Parser[Expression] = (lambda | identifier | "(" ~> expression <~ ")") ~ opt(operands) ^^
    {
      case exp ~ None           => exp
      case exp ~ Some(operands) => FunCall(exp, operands)
    }

  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}" ^^ { make a Block}
  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ exp ~ Nil ~ "}" => Block(List(exp))
    case "{" ~ exp ~ more ~ "}" => Block(exp :: more)
  }

  // lambda ::= "lambda" ~ parameters ~ expression ^^ {make a Lambda}
  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~ exp => Lambda(params, exp)
  }

  // parameters ::= "(" ~ identifier* ~ ")" ^^ { make List[Identifier] }
  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(id ~ Nil) => List(id)
    case Some(id ~ ids) => id :: ids
    case _ => Nil
  }



}
