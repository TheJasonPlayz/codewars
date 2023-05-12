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
  "A language" should "return it's greeting" in {
    val testCases = List(
      ("english", "Welcome"),
      ("dutch", "Welkom"),
      ("IP_ADDRESS_INVALID", "Welcome"),
      ("", "Welcome")
    )
    testCases.foreach({ case (k, v) => assert(Solutions.greet(k) == v) })
  }
  "A list of birds" should "return any birds that are not geese" in {
    val testCases = List[(List[String], List[String])](
      (
        List(
          "Mallard",
          "Hook Bill",
          "African",
          "Crested",
          "Pilgrim",
          "Toulouse",
          "Blue Swedish"
        ),
        List("Mallard", "Hook Bill", "Crested", "Blue Swedish")
      ),
      (
        List("Mallard", "Barbary", "Hook Bill", "Blue Swedish", "Crested"),
        List("Mallard", "Barbary", "Hook Bill", "Blue Swedish", "Crested")
      ),
      (
        List("African", "Roman Tufted", "Toulouse", "Pilgrim", "Steinbacher"),
        List()
      )
    )
    testCases.foreach({ case (k, v) =>
      assert(Solutions.gooseFilter(k) == v)
    })
  }
  "A name" should "return if your playing banjo or not" in {
    val testCases = List( // name, expected
      ("Adam", "Adam does not play banjo"),
      ("Paul", "Paul does not play banjo"),
      ("Ringo", "Ringo plays banjo"),
      ("bravo", "bravo does not play banjo"),
      ("rolf", "rolf plays banjo")
    )

    testCases.foreach { case (k, v) =>
      assert(Solutions.areYouPlayingBanjo(k) == v)
    }
  }
  "A name" should "return first and last intitials, seperated by a dot" in {
    val testCases = List(
      ("Sam Harris", "S.H"),
      ("patrick feenan", "P.F"),
      ("Evan C", "E.C"),
      ("P Favuzzi", "P.F"),
      ("David Mendieta", "D.M")
    )
    testCases.foreach({ case (k, v) => assert(Solutions.abbrevName(k) == v) })
  }
}
