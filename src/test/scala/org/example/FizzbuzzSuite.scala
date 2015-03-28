package org.example

import org.scalatest.FunSuite

class FizzbuzzSuite extends FunSuite {

  def divisibleBy(i: Int): Int => Boolean = { y => y % i == 0 }
  val rules = Map("fizz" -> divisibleBy(3), "buzz" -> divisibleBy(5))

  def fizzbuzz(i: Int) = {
    // ("" /: rules) is a shorter version of rules.foldLeft("")
    val replacements = ("" /: rules){(a:String, t) => if(t._2(i)) a + t._1 else a}
    if (replacements.isEmpty) i else replacements.mkString
  }

//  (1 to 100).foreach{(i) => println(fizzbuzz(i))}

  test("should return 1 on 1")(assert(fizzbuzz(1) == 1))
  test("should return 2 on 2")(assert(fizzbuzz(2) == 2))
  test("should return fizz on 3")(assert(fizzbuzz(3) == "fizz"))
  test("should return buzz on 5")(assert(fizzbuzz(5) == "buzz"))
  test("should return fizzbuzz on 15")(assert(fizzbuzz(15) == "fizzbuzz"))


}
