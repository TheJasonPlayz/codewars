import org.scalatest.flatspec.AnyFlatSpec
import com.solutions.Solutions

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
    testCases.foreach({ case (k, v) =>
      assert(Solutions.greetLanguage(k) == v)
    })
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

  "The function" should "Return 'Hello World'" in {
    assert("hello world!" == Solutions.greetHelloWorld())
  }

  "A string" should "return the string without spaces" in {
    assert(
      Solutions.noSpace(
        "8 j 8   mBliB8g  imjB8B8  jl  B"
      ) === "8j8mBliB8gimjB8B8jlB"
    )
    assert(
      Solutions.noSpace(
        "8 8 Bi fk8h B 8 BB8B B B  B888 c hl8 BhB fd"
      ) === "88Bifk8hB8BB8BBBB888chl8BhBfd"
    )
    assert(Solutions.noSpace("8aaaaa dddd r     ") === "8aaaaaddddr")
    assert(Solutions.noSpace("jfBm  gk lf8hg  88lbe8 ") === "jfBmgklf8hg88lbe8")
    assert(Solutions.noSpace("8j aam") === "8jaam")
  }

  "A list of integers" should "return the smallest integer" in {
    assert(Solutions.findSmallestInt(List(-7, 42)) == -7)
    assert(Solutions.findSmallestInt(List(42, -7)) == -7)
    assert(Solutions.findSmallestInt(List(13, 7, 42)) == 7)
    assert(Solutions.findSmallestInt(List(78, 56, 232, 12, 8)) == 8)
  }

  "Three points of a triangle" should "return the barycenter of the triangle" in {
    val testCases = List[
      ((Int, Int), (Int, Int), (Int, Int), (Double, Double))
    ](
      ((4, 6), (12, 4), (10, 10), (8.6667, 6.6667)),
      ((4, 2), (12, 2), (6, 10), (7.3333, 4.6667)),
      ((4, 8), (8, 2), (16, 6), (9.3333, 5.3333))
    )

    testCases.foreach {
      case (pointA, pointB, pointC, expected) => {
        assert(Solutions.barTriang(pointA, pointB, pointC) == expected)
      }
    }
  }

  "A string containing vowels" should "return a string without vowels" in {
    assert(
      Solutions.disemvowel(
        "No offense but, Your writing is among the worst I've ever read"
      ) == "N ffns bt, Yr wrtng s mng th wrst 'v vr rd"
    )
    assert(
      Solutions.disemvowel(
        "This website is for losers LOL!"
      ) == "Ths wbst s fr lsrs LL!"
    )
  }

  "A list of integers" should "return the sum of the integers' squares" in {
    assert(Solutions.squareSum(List(1, 2)) === 5)
    assert(Solutions.squareSum(List(0, 3, 4, 5)) === 50)
    assert(Solutions.squareSum(List()) === 0)
  }

  "Two strings" should "return true if the first string ends in the second, and false if not" in {
    val testCases = Seq(
      ("samurai", "ai", true),
      ("ninja", "ja", true),
      ("sensei", "i", true),
      ("abc", "abc", true),
      ("abcabc", "bc", true),
      ("fails", "ails", true),
      ("test", "", true),
      ("sumo", "omo", false),
      ("samurai", "ra", false),
      ("abc", "abcd", false),
      ("ails", "fails", false),
      ("this", "fails", false),
      ("spam", "eggs", false)
    )

    testCases.foreach { case (s, e, expected) =>
      assert(expected == Solutions.stringEndsWith(s, e))
    }
  }

  "A string" should "return the integer form of the string" in {
    assert(1234 == Solutions.stringToNumber("1234"))
    assert(605 == Solutions.stringToNumber("605"))
    assert(1405 == Solutions.stringToNumber("1405"))
    assert(-7 == Solutions.stringToNumber("-7"))
  }
}
