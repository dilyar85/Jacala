package ui

import scala.util.parsing.combinator._

/**
  * Created by Dilyar on 4/20/17.
  */
class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")