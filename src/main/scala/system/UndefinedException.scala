package system

import expression.Identifier

/**
  * Created by Dilyar on 4/20/17.
  */
class UndefinedException(name: Identifier) extends JediException("Undefined identifier: " + name)
