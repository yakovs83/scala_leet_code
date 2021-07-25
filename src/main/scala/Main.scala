import open_the_lock.Solution._

object Main extends App {
  //val deadends = Array("0201", "0101", "0102", "1212", "2002")
  val deadends =
    Array("8887", "8889", "8878", "8898", "8788", "8988", "7888", "9888")
  val target = "8888"
  val res = openLock(deadends, target)
  println(s"$res")
}
