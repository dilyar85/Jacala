package system

import value.Environment

/**
  * Ework Console object. Most of the codes in this program are given by the professor during the lecture
  */
object console {
  val globalEnv = new Environment // Created here
  val parsers = new WookieParsers // for now
  var verbose = false


  def execute(cmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmd)
    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _ =>
        val exp = tree.get
        // get the expression from the tree
        val result = exp.execute(globalEnv) // execute the expression
        result.toString // return string representation of result
    }
  }

  def repl {
    var more = true
    var cmd: String = ""
    while (more) {
      try {
        // read/execute/print
        print("-> ")
        cmd = readLine
        // handle meta-commands
        if (cmd == "quit") more = false
        else println(execute(cmd))
      }
      catch {
        case e: SyntaxException => {
          println(e.getMessage)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        case e: UndefinedException => {
          println(e)
          if (verbose) e.printStackTrace()
        }
        case e: JediException => {
          println(e)
        }
        case e: Exception => {
          println(e)
          more = false
        }
        // handle other types of exceptions

      } finally {
        Console.flush
      }
    }
    println("Bye.")
  }

  def main(args: Array[String]): Unit = {
    repl
  }
}
