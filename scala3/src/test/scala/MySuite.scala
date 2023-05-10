import org.scalatest.flatspec.AnyFlatSpec
import com.solutions.Solutions
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Tests extends AnyFlatSpec {
  "1 and 2" should "return Odd then Even" in {
    val (odd, even) = (Solutions.evenOrOdd(1), Solutions.evenOrOdd(2))
    assert(odd == "Odd" && even == "Even")
  }
}
