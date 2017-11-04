package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ListSpec extends FunSuite {

  test("Exercise 3.2 - tail") {
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  test("Exercise 3.3 - setHead") {
    setHead(List(1, 2, 3), 5) shouldBe List(5, 2, 3)
  }

  test("Exercise 3.4 - drop") {
    drop(List(1, 2, 3), 2) shouldBe List(3)
  }

  test("Exercise 3.5 - dropWhile") {
    dropWhile[Int](List(1, 2, 3), _ < 2) shouldBe List(2, 3)
  }

  test("Exercise 3.6 - init") {
    init(List(1, 2, 3)) shouldBe List(1, 2)
  }

  test("Exercise 3.7 - short-circuited foldRight") {
    product3(List(1, 2, 3, 0.0, 4)) shouldBe 0.0
  }

  test("Exercise 3.8 - list constructor vs foldRight") {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
  }

  test("Exercise 3.9 - length") {
    List.length(List(1, 2, 3)) shouldBe 3
  }

  test("Exercise 3.10 - foldLeft") {
    foldLeftNotTailRecursive(List(1, 2, 3), 0)(_ + _) shouldBe 6
    foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
  }

  test("Exercise 3.11 - sum and product using foldLeft") {
    foldLeftSum(List(1, 2, 3)) shouldBe 6
    foldLeftProduct(List(5, 2, 3)) shouldBe 30
  }

  test("Exercise 3.12 - reverse") {
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  test("Exercise 3.13 - foldLeft in terms of foldRight and vice versa") {
    foldLeftInTermsOfFoldRight(List(1, 2, 3), 0)(_ + _) shouldBe 6
    foldRightInTermsOfFoldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
  }

  test("Exercise 3.14 - append in terms of foldRight") {
    appendInTermsOfFoldRight(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  test("Exercise 3.15 - flatten") {
    flatten(List(List(1, 2), List(3, 4), List(5, 6, 7))) shouldBe List(1, 2, 3, 4, 5, 6, 7)
  }

  test("Exercise 3.16 - add one to each element") {
    plusOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  test("Exercise 3.17 - convert each double to string in list") {
    doubleToString(List(1.0, 2.0, 3.0)) shouldBe List("1.0", "2.0", "3.0")
  }

  test("Exercise 3.18 - map") {
    map(List(1, 2, 3))(_ + 42) shouldBe List(43, 44, 45)
  }

  test("Exercise 3.19 - filter") {
    filter(List(1, 2, 0, 5))(_ % 2 == 0) shouldBe List(2, 0)
  }

  test("Exercise 3.20 - flatMap") {
    flatMap(List(1, 2, 3))(a => List(a, a + 1)) shouldBe List(1, 2, 2, 3, 3, 4)
  }

  test("Exercise 3.21 - filter using flatMap") {
    filterUsingFlatMap(List(1, 2, 0, 5))(_ % 2 == 0) shouldBe List(2, 0)
  }

  test("Exercise 3.22 - merge lists") {
    merge(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
    merge(List(1, 2, 3, 10), List(4, 5, 6)) shouldBe List(5, 7, 9, 10)
    merge(List(1, 2, 3), List(4, 5, 6, 10)) shouldBe List(5, 7, 9, 10)
  }

  test("Exercise 3.23 - zipWith") {
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _) shouldBe List(4, 10, 18)
  }

  test("Exercise 3.24 - hasSubsequence") {
    hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(3, 4)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(3, 5)) shouldBe false
    hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(1)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(3)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(7)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(6, 7, 8)) shouldBe false
    hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List()) shouldBe true
    hasSubsequence(List(), List()) shouldBe true
  }

}
