package fpinscala.errorhandling

import fpinscala.errorhandling.Option._
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class OptionSpec extends FunSuite {

  test("Exercise 4.1 - Option interface") {
    Some(42).map(_.toString) shouldBe Some("42")
    None.map(_.toString) shouldBe None

    Some(42).getOrElse(3) shouldBe 42
    None.getOrElse(3) shouldBe 3

    Some(42).flatMap(x => Some(x.toString)) shouldBe Some("42")
    Some(42).flatMap(_ => None) shouldBe None
    None.flatMap(x => Some(x.toString)) shouldBe None
    None.flatMap(_ => None) shouldBe None

    Some(42).orElse(Some(3)) shouldBe Some(42)
    Some(42).orElse(None) shouldBe Some(42)
    None.orElse(Some(3)) shouldBe Some(3)
    None.orElse(None) shouldBe None

    Some(42).filter(_ == 42) shouldBe Some(42)
    Some(42).filter(_ == 3) shouldBe None
    None.filter(_ == 42) shouldBe None
  }

  test("Exercise 4.2 - variance") {
    variance(List(1.0, 2.0, 3.0, 4.0)) shouldBe Some(1.25)
    variance(List.empty) shouldBe None
  }

  test("Exercise 4.3 - map2") {
    map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
    map2(Some(1), None)(_ + _) shouldBe None
    map2[Int, Int, Int](None, Some(2))(_ + _) shouldBe None
    map2[Int, Int, Int](None, None)(_ + _) shouldBe None
  }

  test("Exercise 4.4 - sequence") {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequence(List()) shouldBe Some(List())
    sequence(List(Some(1), None)) shouldBe None
    sequence(List(None)) shouldBe None

    sequenceFoldRight(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequenceFoldRight(List()) shouldBe Some(List())
    sequenceFoldRight(List(Some(1), None)) shouldBe None
    sequenceFoldRight(List(None)) shouldBe None
  }

  test("Exercise 4.5 - traverse") {
    def Try[A, B](f: A => B)(a: A): Option[B] = try { Some(f(a)) } catch { case _: Exception => None }
    traverse(List("1", "2", "3"))(Try(_.toInt)) shouldBe Some(List(1, 2, 3))
    traverse(List("1", "2", "x"))(Try(_.toInt)) shouldBe None
    traverse(List.empty[String])(Try(_.toInt)) shouldBe Some(List())
  }

  test("Exercise 4.4 (reprise) - sequence implemented using traverse") {
    sequenceUsingTraverse(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequenceUsingTraverse(List()) shouldBe Some(List())
    sequenceUsingTraverse(List(Some(1), None)) shouldBe None
    sequenceUsingTraverse(List(None)) shouldBe None
  }

}
