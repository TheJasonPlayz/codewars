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
}
