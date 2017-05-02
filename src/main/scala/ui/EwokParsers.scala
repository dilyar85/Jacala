package ui

import expression._
import value.{Boole, Number}
import scala.util.parsing.combinator._

/**
  * EworkParsers, most of the codes are given by the professor during the lecture
  */
class EwokParsers extends RegexParsers {


  //expression ::= declaration | conditional | disjunction
  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")


  //declaration ::= "def"~identifier~"="~expression
  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
  }

  //conditional ::= "if"~"("~expression~")"~expression~("else"~expression)?
  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if" ~ "(" ~ exp1 ~ ")" ~ exp2 ~ None => Conditional(exp1, exp2)
    case "if" ~ "(" ~ exp1 ~ ")" ~ exp2 ~ Some("else" ~ exp3) => Conditional(exp1, exp2, exp3)
  }

  //  def boole: Parser[Any] = "true" | "false"
  //  def conjunction: Parser[Any] = boole ~ opt("&&" ~ boole)
  //  def disjunction: Parser[Any] = boole ~ rep("||" ~ boole)

  //disjunction ::= conjunction~("||"~conjunction)*
  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ cons => Disjunction(con :: cons)

  }

  //conjunction ::= equality~("&&"~equality)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case eql ~ Nil => eql
    case eql ~ eqls => Conjunction(eql :: eqls)
  }

  //equality ::= inequality~("=="~inequality)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case ineql ~ Nil => ineql
    case ineql ~ ineqls => FunCall(Identifier("equals"), ineql :: ineqls)
  }

  //inequality ::= sum ~ ("<" ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case sum ~ None => sum
    case sum ~ Some("<" ~ other) => FunCall(Identifier("less"), List(sum, other))
    case sum ~ Some(">" ~ other) => FunCall(Identifier("more"), List(sum, other))
    case sum ~ Some("!=" ~ other) => FunCall(Identifier("unequals"), List(sum, other))
  }

  //sum ::= product ~ (("+" | "-") ~ product)*
  def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ {
    case "+" ~ product => product
    case "-" ~ product => negate(product)
  }) ^^ {
    case p ~ Nil => p
    case p ~ rest => FunCall(Identifier("add"), p :: rest)

  }

  //product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*" | "/") ~ term ^^ {
    case "*" ~ term => term
    case "/" ~ term => FunCall(Identifier("div"), List(Number(1.0), term))
  }) ^^ {
    case term ~ Nil => term
    case term ~ terms => FunCall(Identifier("mul"), term :: terms)
  }

  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }


  //term ::= funcall | identifier | number | boole | "("~expression~")"
  def term: Parser[Expression] = literal | funcall | identifier | "(" ~> expression <~ ")"

  //funcall ::= identifier~operands
  def funcall: Parser[Expression] = identifier ~ operands ^^ {
    case id ~ operands => FunCall(id, operands)
  }

  //identifier ::= [a-zA-Z][a-zA-Z0-9]*
  def identifier: Parser[Identifier] =
    """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
      case someString => Identifier(someString)
    }

  //LITERAL ::= BOOLE | NUMBER
  def literal: Parser[Literal] = boole | number

  //number ::= [1-9][0-9]*("."[0-9]+)?
  def number: Parser[Literal] =
    """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ {
      case someString => Number(someString.toDouble)
    }

  //boole ::= "true" | "false"
  def boole: Parser[Boole] =
    """true|false|!true|!false""".r ^^ {
      case "true" => Boole(true)
      case "!true" => Boole(false)
      case "false" => Boole(false)
      case "!false" => Boole(true)
    }

  //operands ::= "("~(expression ~ (","~expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~ opt(expression ~ rep("," ~> expression)) ~ ")" ^^ {
    //~> means "What is front of me, drop"
    case "(" ~ None ~ ")" => Nil
    case "(" ~ Some(exp ~ Nil) ~ ")" => List(exp)
    case "(" ~ Some(exp ~ exps) ~ ")" => exp :: exps
  }

}
