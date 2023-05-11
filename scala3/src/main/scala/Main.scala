package com.solutions
object Solutions {

  /** @param number
    *   Number to check if even or odd
    * @return
    *   "Even" if number is even, else "Odd"
    */
  def evenOrOdd(number: Int): String =
    if (number % 2 == 0) "Even" else "Odd"

  /** @param s
    *   String to manipulate
    * @return
    *   String with first and last characters removed
    */
  def removeChars(s: String): String =
    s.substring(1, s.length - 1)

  /** @param arr
    *   Array to sum up
    * @param acc
    *   Accumulator cache
    * @return
    *   Sum of all positives in an array
    */
  def positiveSum(arr: Array[Int], acc: Int = 0): Int = arr.length match {
    case 0 => 0
    case 1 => if (arr.head > 0) acc + arr.head else acc
    case _ =>
      if (arr.head > 0) positiveSum(arr.tail, acc + arr.head)
      else positiveSum(arr.tail, acc)
  }

  def greet(language: String): String = {
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
}
