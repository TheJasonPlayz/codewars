import org.scalatest.flatspec.AnyFlatSpec
import com.solutions.Solutions
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Tests extends AnyFlatSpec {
  "1 and 2" should "return Odd then Even" in {
    val (odd, even) = (Solutions.evenOrOdd(1), Solutions.evenOrOdd(2))
    assert(odd == "Odd" && even == "Even")
  }
  "A string" should "return itself without first and last chars" in {
    val tests = scala.collection.immutable.Map[String, String](
      "eloquent" -> "loquen",
      "country" -> "ountr",
      "person" -> "erso",
      "place" -> "lac"
    )
    tests.foreach({ case (k, v) =>
      assert(
        Solutions.removeChars(k) == v
      )
    })
  }
  "An array of integers" should "return the sum of all positives in array" in {
    val test = Array(1, 2, 3, 4, 5, -1, -2, -3, -4, -5)
    assert(Solutions.positiveSum(test) == 15)
  }
}
