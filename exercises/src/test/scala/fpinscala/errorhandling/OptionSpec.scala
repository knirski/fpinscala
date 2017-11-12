package fpinscala.errorhandling

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

}
