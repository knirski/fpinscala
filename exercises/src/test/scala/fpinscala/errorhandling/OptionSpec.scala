package fpinscala.datastructures

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class OptionSpec extends FunSuite {

  private def some[A](value: A) = fpinscala.errorhandling.Some(value)
  private val none = fpinscala.errorhandling.None

  test("Exercise 4.1 - Option interface") {
    some(42).map(_.toString) shouldBe some("42")
    none.map(_.toString) shouldBe none

    some(42).getOrElse(3) shouldBe 42
    none.getOrElse(3) shouldBe 3

    some(42).flatMap(x => some(x.toString)) shouldBe some("42")
    some(42).flatMap(_ => none) shouldBe none
    none.flatMap(x => some(x.toString)) shouldBe none
    none.flatMap(_ => none) shouldBe none

    some(42).orElse(some(3)) shouldBe some(42)
    some(42).orElse(none) shouldBe some(42)
    none.orElse(some(3)) shouldBe some(3)
    none.orElse(none) shouldBe none

    some(42).filter(_ == 42) shouldBe some(42)
    some(42).filter(_ == 3) shouldBe none
    none.filter(_ == 42) shouldBe none
  }

}
