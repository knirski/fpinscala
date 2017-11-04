package fpinscala.datastructures

import fpinscala.datastructures.Tree._
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TreeSpec extends FunSuite {

  private val leaf = Leaf(1)
  private val threeLeafs = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  private val fiveLeafs = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))

  test("Exercise 3.25 - size") {
    Tree.size(leaf) shouldBe 1
    Tree.size(threeLeafs) shouldBe 3
    Tree.size(fiveLeafs) shouldBe 5
  }

  test("Exercise 3.26 - maximum") {
    maximum(leaf) shouldBe 1
    maximum(threeLeafs) shouldBe 3
    maximum(fiveLeafs) shouldBe 5
  }

  test("Exercise 3.27 - depth") {
    depth(leaf) shouldBe 0
    depth(threeLeafs) shouldBe 2
    depth(fiveLeafs) shouldBe 3
  }

  test("Exercise 3.28 - map") {
    map(leaf)(_ + 1) shouldBe Leaf(2)
    map(leaf)(_ * 5) shouldBe Leaf(5)
    map(threeLeafs)(_.toString) shouldBe Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
  }

  test("Exercise 3.29 - fold") {
    sizeFold(leaf) shouldBe 1
    sizeFold(threeLeafs) shouldBe 3
    sizeFold(fiveLeafs) shouldBe 5

    maximumFold(leaf) shouldBe 1
    maximumFold(threeLeafs) shouldBe 3
    maximumFold(fiveLeafs) shouldBe 5

    depthFold(leaf) shouldBe 0
    depthFold(threeLeafs) shouldBe 2
    depthFold(fiveLeafs) shouldBe 3

    mapFold(leaf)(_ + 1) shouldBe Leaf(2)
    mapFold(leaf)(_ * 5) shouldBe Leaf(5)
    mapFold(threeLeafs)(_.toString) shouldBe Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
  }

}
