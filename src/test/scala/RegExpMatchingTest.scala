import org.scalatest.FlatSpec
import org.scalatest.Assertions._
import regexp_matching.Solution._

class RegExpMatchingTest extends FlatSpec {
  val tests = List(
    ("", "", true, "empty string, empty pattern"),
    ("", "a*", true, "empty string, char wildcard"),
    ("", ".*", true, "empty string, any char wildcard"),
    ("", ".*c*", true, "empty string, multiple any* wildcards"),
    ("a", "a", true, "single char match"),
    ("a", ".", true, "any char match"),
    ("a", "a*", true, "wildcard match"),
    ("aa", "a*", true, "multichar wildcard match"),
    ("aab", "c*a*b", true, "multichar wildcard match"),
    ("a", ".*", true, "non-empty string any char wildcard"),
    ("abc", "abc", true, "exact match, multiple chars")
  )
  behavior of "isMatch"
  tests.foreach {
    case (s, p, res, desc) =>
      it should s"${if (res) "" else "not"} work for $desc: $s vs $p" in {
        assert(isMatch(s, p) == res)
      }
  }
  behavior of "isMatch2"
  tests.foreach {
    case (s, p, res, desc) =>
      it should s"${if (res) "" else "not"} work for $desc: $s vs $p" in {
        assert(isMatch2(s, p) == res)
      }
  }
  // "isMatch" should "work for a single letter match" in {
  //   assert(isMatch("a", "a"))
  // }
}
