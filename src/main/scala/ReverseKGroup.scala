package reverse_k_group
class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
  override def toString(): String = x.toString + " -> " + {
    if (next == null) "null" else next.toString
  }
}

object Solution {
  def from_list(l: List[Int]): ListNode = {
    l.foldRight(null: ListNode)((x, node) => {
      val tmp = new ListNode(x); tmp.next = node; tmp
    })
  }
  implicit class ListNodeOps(l: ListNode) {
    def take(k: Int): (ListNode, Int) = {
      var cur = l
      var n = 0
      while (n < k && cur != null) {
        n += 1
        cur = cur.next
      }
      (cur, n)
    }
  }
  def reverseKGroup(head: ListNode, k: Int): ListNode = {
    if (head == null || k == 1) head
    else
      head.take(k) match {
        case (nh, n) if n == k => {
          var tail = reverseKGroup(nh, k)
          var cnt = 0
          var cur = head
          var next = cur.next
          while (cnt < k) {
            cur.next = tail
            tail = cur
            cur = next
            next = if (cur != null) cur.next else null
            cnt += 1
          }
          tail
        }
        case (nh, _) => head
      }
  }
}
