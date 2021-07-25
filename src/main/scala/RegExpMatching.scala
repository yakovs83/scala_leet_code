package regexp_matching
sealed trait Re
final case class RepC(c: Char) extends Re
final case class RepAny() extends Re
final case class C(c: Char) extends Re
final case class Any() extends Re

object Solution {
  def isMatch(s: String, p: String): Boolean = {
    def tokenize(p: List[Char]): List[Re] = {
      p match {
        case '.' :: '*' :: t => RepAny() :: tokenize(t)
        case c :: '*' :: t   => RepC(c) :: tokenize(t)
        case '.' :: t        => Any() :: tokenize(t)
        case c :: t          => C(c) :: tokenize(t)
        case Nil             => Nil
      }
    }
    val pattern = tokenize(p.toList)
    def dpMatch(s: List[Char], p: List[Re]): Boolean = {
      val cache = collection.mutable.Map.empty[(Int, Int), Boolean]
      def simpleIsMatch(s: List[Char], p: List[Re]): Boolean = {
        val k = (s.length, p.length)
        cache.getOrElse(
          k, {
            cache.update(
              k,
              (s, p) match {
                case (c1 :: t1, C(c2) :: t2) =>
                  c1 == c2 && simpleIsMatch(t1, t2)
                case (c1 :: t1, Any() :: t2) => simpleIsMatch(t1, t2)
                case (c1 :: t1, pp @ RepC(c2) :: t2) =>
                  (c1 == c2 && simpleIsMatch(t1, pp)) || simpleIsMatch(
                    c1 :: t1,
                    t2
                  )
                case (c1 :: t1, pp @ RepAny() :: t2) =>
                  simpleIsMatch(t1, pp) || simpleIsMatch(
                    c1 :: t1,
                    t2
                  )
                case (c1 :: t1, Nil) => false
                case (Nil, p2 :: t2) =>
                  p2 match {
                    case RepC(_) | RepAny() =>
                      simpleIsMatch(s, t2)
                    case Any() | C(_) => false
                  }
                case (Nil, Nil) => true
              }
            )
            cache(k)
          }
        )
      }
      simpleIsMatch(s, p)
    }
    dpMatch(s.toList, pattern)
  }

  def isMatch2(s: String, p: String): Boolean = {
    val sl = s.toList
    val pl = p.toList
    val cache = collection.mutable.Map.empty[(Int, Int), Boolean]
    def dpMatch(s: List[Char], p: List[Char]): Boolean = {
      val k = (s.length, p.length)
      cache.getOrElse(
        k, {
          cache.update(
            k,
            (s, p) match {
              case (s1 @ c1 :: t1, p2 @ c2 :: '*' :: t2)
                  if (c1 == c2 || c2 == '.') =>
                dpMatch(t1, p2) || dpMatch(s1, t2)
              case (s1, _ :: '*' :: t2) => dpMatch(s1, t2)
              case (c1 :: t1, c2 :: t2) =>
                if (c1 == c2 || c2 == '.') dpMatch(t1, t2) else false
              case (Nil, _ :: _) | (_ :: _, Nil) => false
              case (Nil, Nil)                    => true
            }
          )
          cache(k)
        }
      )
    }
    dpMatch(sl, pl)
  }
}
