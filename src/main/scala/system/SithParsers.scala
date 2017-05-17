package system

import expression._
import value.{Boole, Number}

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Dilyar on 5/16/17.
  */
class SithParsers extends RegexParsers {


  def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")

  def iteration: Parser[Iteration] = "while" ~> "(" ~ expression ~ ")" ~ expression ^^ {
    case "(" ~ con ~ ")" ~ exp => Iteration(con, exp)
  }

  // dereference variables, e.g. [x]
  def deref: Parser[Expression] = "[" ~> expression <~ "]" ^^ {
    case exp => FunCall(Identifier("content"), List(exp))
  }

  // assign a variable: x = blah
  def assignment: Parser[Expression] = identifier ~ "=" ~ expression ^^ {
    case id ~ "=" ~ exp => Assignment(id, exp)
  }

  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}" ^^ { make a Block}
  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ exp ~ Nil ~ "}" => Block(List(exp))
    case "{" ~ exp ~ more ~ "}" => Block(exp :: more)
  }

  // lambda ::= "lambda" ~ parameters ~ expression ^^ {make a Lambda}
  def lambda: Parser[Expression] = "lambda" ~> parameters ~ expression ^^ {
    case params ~ exp => Lambda(params, exp)
  }

  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(id ~ Nil) => List(id)
    case Some(id ~ ids) => id :: ids
    case _ => Nil
  }

  //operands ::= "("~(expression ~ (","~expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
    //~> means "What is front of me, drop"
    case None => Nil
    case Some(exp ~ Nil) => List(exp)
    case Some(exp ~ exps) => exp :: exps
  }

  def funcall: Parser[Expression] = identifier ~ opt(operands) ^^ {
    case id ~ None => id
    case id ~ Some(operands) => FunCall(id, operands)
  }

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

  def term: Parser[Expression] = deref | assignment | lambda | block | funcall | literal | identifier | "(" ~> expression <~ ")"

  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
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
    """(\+|-)?(0|[1-9][0-9]*)(\.[0-9]+)?""".r ^^ {
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


}
