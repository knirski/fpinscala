package fpinscala.gettingstarted

import fpinscala.gettingstarted.MyModule._
import fpinscala.gettingstarted.PolymorphicFunctions._
import org.scalatest.{FunSuite, Matchers}

import scala.util.Random

class GettingStartedSpec extends FunSuite with Matchers {

  test("Exercise 2.1 - fibonacci numbers") {
    fib(0) shouldBe 0
    fib(1) shouldBe 1
    fib(2) shouldBe 1
    fib(9) shouldBe 34
    fib(13) shouldBe 233
  }

  test("Exercise 2.2 - isSorted") {
    val as = Seq.fill(20)(Random.nextInt).toArray
    as.sorted shouldBe sorted
    isSorted[Int](as.sorted, _ < _) shouldBe true
  }

  test("Exercise 2.3 - curry") {
    val oldFunction: (Int, Int) => Int = (a, b) => a + b
    val newFunction: Int => Int => Int = curry(oldFunction)
    newFunction(3)(4) shouldBe oldFunction(3, 4)
  }

  test("Exercise 2.4 - uncurry") {
    val oldFunction: Int => Int => Int = a => b => a + b
    val newFunction: (Int, Int) => Int = uncurry(oldFunction)
    newFunction(3, 4) shouldBe oldFunction(3)(4)
  }

  test("Exercise 2.5 - compose") {
    val addOne: Int => Int = _ + 1
    val addTwo: Int => Int = _ + 2
    compose(addOne, addTwo)(9) shouldBe 12
    compose(addTwo, addOne)(9) shouldBe 12
  }

}
