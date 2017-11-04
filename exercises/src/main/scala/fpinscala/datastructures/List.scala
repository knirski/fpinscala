package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(oldHead, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case as@Cons(h, t) => if (n == 0) as else drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case as@Cons(h, t) => if (f(h)) dropWhile(t, f) else as
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRightShortCircuit[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) =>
      val computedFromTail = foldRightShortCircuit(t, z)(f)
      if (computedFromTail == z) z else f(h, computedFromTail)
  }

  def product3(ns: List[Double]): Double = foldRightShortCircuit(ns, 0.0)(_ * _)

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => 1 + b)

  def foldLeftNotTailRecursive[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(foldLeftNotTailRecursive(t, z)(f), h)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(l: List[A], z: B, acc: B = z)(f: (B, A) => B): B = l match {
      case Nil => acc
      case Cons(h, t) => loop(t, z, f(acc, h))(f)
    }
    loop(l, z)(f)
  }

  def foldLeftSum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def foldLeftProduct(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))

  def foldLeftInTermsOfFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(foldRight(t, z)((b, a) => f(a, b)), h)
  }

  def foldRightInTermsOfFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldLeft(t, z)((a, b) => f(b, a)))
  }

  def appendInTermsOfFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(h, flatten(t))
  }

  def plusOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, plusOne(t))
  }

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doubleToString(t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def merge(as: List[Int], as2: List[Int]): List[Int] = as match {
    case Nil => as2
    case whole@Cons(h, t) => as2 match {
      case Nil => whole
      case Cons(h2, t2) => Cons(h + h2, merge(t, t2))
    }
  }

  def zipWith[A](as: List[A], as2: List[A])(f: (A, A) => A): List[A] = as match {
    case Nil => as2
    case whole@Cons(h, t) => as2 match {
      case Nil => whole
      case Cons(h2, t2) => Cons(f(h, h2), zipWith(t, t2)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def inner(sup: List[A], sub: List[A], started: Boolean = false): Boolean = sup match {
       case Nil => sub == Nil
       case Cons(h, t) => sub match {
         case Nil => true
         case Cons(sh, st) =>
           if (h == sh) inner(t, st, true)
           else if (!started) inner(t, sub)
           else false
       }
    }
    inner(sup, sub)
  }

}
