package value

/**
  * Created by Dilyar on 4/25/17.
  */
class Notification(val msg: String) extends Value {
  override def toString: String = msg
}

object Notification {
  def apply(msg: String) = new Notification(msg)

  val OK = Notification("ok")
  val DONE = Notification("done")
  val FAIL = new Notification("fail")
  //etc.
}