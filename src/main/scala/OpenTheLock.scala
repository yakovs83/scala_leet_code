package open_the_lock
import scala.collection.mutable.{HashMap => MMap}
import scala.collection.mutable.{Queue => MQueue}
import scala.collection.mutable.{HashSet => MSet}
import java.util.HashSet
object Solution {
  implicit class IntIncDec(i: Int) {
    def inc = if (i + 1 > 9) 0 else i + 1
    def dec = if (i - 1 < 0) 9 else i - 1
  }
  implicit class StrDigits(s: String) {
    def asDigits = s.toCharArray().map(_.asDigit)
  }
  def neighbors(s: String): List[String] = {
    val digits = s.asDigits
    var res: List[String] = List()
    for (i <- 0 until 4) {
      val (x1, x2) = (digits.clone(), digits.clone())
      x1(i) = x1(i).inc
      x2(i) = x2(i).dec
      res = x1.mkString :: x2.mkString :: res
    }
    res
  }
  def openLock(deadends: Array[String], target: String): Int = {
    val visited: MMap[String, Int] = MMap(("0000", 0))
    val blocked: MSet[String] = MSet.concat(deadends)
    if (blocked.contains("0000")) return -1
    val queue: MQueue[(String, Int)] = MQueue(("0000", 0))
    while (queue.size > 0) {
      val (pos, dist) = queue.dequeue
      val ndist = dist + 1
      neighbors(pos)
        .filter(!blocked.contains(_))
        .foreach(elem => {
          if (
            !visited.contains(elem) || (visited
              .contains(elem) && visited(elem) > ndist)
          ) {
            visited(elem) = ndist
            queue.enqueue((elem, ndist))
          }
        })
    }
    visited.getOrElse(target, -1)
  }
}
