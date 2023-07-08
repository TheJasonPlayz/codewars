package com.solutions

object Solutions {

  /** @param number
    *   Number to check if even or odd
    * @return
    *   "Even" if @param number is even, else "Odd"
    */
  def evenOrOdd(number: Int): String =
    if (number % 2 == 0) "Even" else "Odd"

  /** @param s
    *   String to manipulate
    * @return
    * @param s
    *   with first and last characters removed
    */
  def removeChars(s: String): String =
    s.substring(1, s.length - 1)

  /** @param arr
    *   Array to sum
    * @param acc
    *   Accumulator cache
    * @return
    *   Sum of all positives in @param arr
    */
  def positiveSum(arr: Array[Int], acc: Int = 0): Int = arr.length match {
    case 0 => 0
    case 1 => if (arr.head > 0) acc + arr.head else acc
    case _ =>
      if (arr.head > 0) positiveSum(arr.tail, acc + arr.head)
      else positiveSum(arr.tail, acc)
  }

  /** @param language
    *   Langauge to be greeted from
    * @return
    *   Greeting from @param language
    */
  def greetLanguage(language: String): String = {
    val languages = scala.collection.immutable.Map[String, String](
      "english" -> "Welcome",
      "czech" -> "Vitejte",
      "danish" -> "Velkomst",
      "dutch" -> "Welkom",
      "estonian" -> "Tere tulemast",
      "finnish" -> "Tervetuloa",
      "flemish" -> "Welgekomen",
      "french" -> "Bienvenue",
      "german" -> "Willkommen",
      "irish" -> "Failte",
      "italian" -> "Benvenuto",
      "latvian" -> "Gaidits",
      "lithuanian" -> "Laukiamas",
      "polish" -> "Witamy",
      "spanish" -> "Bienvenido",
      "swedish" -> "Valkommen",
      "welsh" -> "Croeso"
    )
    languages.getOrElse(language, "Welcome")
  }

  /** @param birds
    *   Array of birds
    * @return
    * @param birds
    *   without any geese
    */
  def gooseFilter(birds: List[String]): List[String] = {
    val goose =
      List("African", "Roman Tufted", "Toulouse", "Pilgrim", "Steinbacher")
    birds.filter(b => !goose.contains(b))
  }

  /** @param name
    *   Name to enter
    * @return
    *   If person plays banjo, based on @param name
    */
  def areYouPlayingBanjo(name: String) = name.head match {
    case 'R' | 'r' => name + " plays banjo"
    case _         => name + " does not play banjo"
  }

  /** @param name
    *   Name to abbreviate
    * @return
    *   first and last initials of @param name seperated by a dot
    */
  def abbrevName(name: String): String = {
    import scala.util.matching.Regex
    val pattern = "^(.).*\\s(.).*".r
    name match {
      case pattern(first, last) => {
        s"${first.toUpperCase}.${last.toUpperCase()}"
      }
    }
  }

  def greetHelloWorld(): String =
    "hello world!".reverse
      .toCharArray()
      .reverse
      .mkString
      .reverse
      .toCharArray()
      .reverse
      .mkString

  /** @param s
    *   String to remove spaces from
    * @return
    * @param s
    *   with spaces removed
    */
  def noSpace(s: String): String = s.filter(c => c != ' ')

  /** @param nums
    *   Array to find the smallest integer from
    * @return
    *   Smallest integer of @param nums
    */
  def findSmallestInt(nums: List[Int]): Int = nums.min

  /** @param pointA
    *   First point to find barycenter
    * @param pointB
    *   Second point to find barycenter
    * @param pointC
    *   Third point to find barycenter
    * @return
    *   Barycenter of a triangle represented by a list of doubles made of @param
    *   pointA, @param pointB, and @param pointC
    */
  def barTriang(
      pointA: (Int, Int),
      pointB: (Int, Int),
      pointC: (Int, Int)
  ): (Double, Double) = (
    ((((pointA._1 + pointB._1 + pointC._1) / 3.0) * 10000).round / 10000.0),
    ((((pointA._2 + pointB._2 + pointC._2) / 3.0) * 10000).round / 10000.0)
  )

  /** @param str
    *   String to remove vowels from
    * @return
    *   String without vowels
    */
  def disemvowel(str: String): String = {
    val v = scala.collection.immutable.List[Char](
      'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'
    )
    str.filter(c => !v.contains(c))
  }

  /** @param xs
    *   List of integers to sum the squares of
    * @return
    *   The sum of each integers' square in @param xs
    */
  def squareSum(xs: List[Int]): Int = xs.map(c => c * c).sum

  /** @param s
    *   String to check for the end of @param e
    * @param e
    *   String to check if it is in the end of @param s
    * @return
    *   True if @param s ends with @param e, false if not
    */
  def stringEndsWith(s: String, e: String): Boolean =
    if (s.slice(s.length - e.length, s.length) == e) true else false

  /** @param s
    *   String to turn into a number
    * @return
    *   turns @param s into a number
    */
  def stringToNumber(s: String): Int = s.toInt
}
