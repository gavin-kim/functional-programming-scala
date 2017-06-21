package c3_FunctionalDataStructure

/**
  * Keywords
  *
  * trait: An abstract interface that may optionally contain implementations of some methods
  * sealed: All implementations must be declared in this file.
  * case: Implementations or data constructors to represent possible forms the List can take
  *
  * Cons[]: traditionally short for construct
  * Nil: represents an empty list of anything of zero length.
  *
  * +A is a variance annotation that signals that A is a covariant or "positive" parameter of List.
  * If X is a subtype of Y, then List[X] is a subtype of List[Y].
  * Nil extends List[Nothing] and Nothing is subtype of all types.
  * So Nil can be considered a List[Int] or List[Double] etc...
  *
  *
  * Pattern matching
  *
  * case _ :any expression and you don't need to capture _
  * case x : any expression but you can capture variable x
  * case Cons(h, _) : to capture the first item in a list
  * case Cons(_, t) : to capture the last item in a list
  * case Nil : MatchError at runtime
  * Cons(x1, Cons(x2, Nil)) and Cons(y1, Cons(y2, Cons(y3, _))) are valid patterns
  *
  *
  * Data sharing in functional data structures
  *
  * [a] -> [b] -> [c] -> [d]
  *  |      |
  *  |      ----------------------------
  *  |                                 |
  *  List("a","b","c","d") -> .tail -> List("b","c","d")
  *
  * Lists are immutable, We don't need to actually copy xs; We can just reuse it.
  * Functional data structures are persistent, meaning that existing references
  * are never changed by operations on the data structure.
  *
  *
  * In Scala. all methods whose names end in ':' are right-associative.
  * Expression x :: xs is actually xs.::(x). It calls constructor ::(x, xs)
  */

/** List data type, parameterized on a type, A. */
sealed trait List[+A]

/** Data constructors of List */
case object Nil extends List[Nothing] // List can be empty, denoted by the data constructor Nil
case class Cons[+A] (head: A, tail: List[A]) extends List[A] // A nonempty list

/** Declare a companion object*/
object List {

  /** A function that uses pattern matching to add up a list of integers. */
  def sum (ints: List[Int]): Int =
    ints match { // pattern matching for the parameter ints
      case Nil => 0 // The sum of the empty list = 0
      case Cons(x, xs) => x + sum(xs)
    }

  def sum2(ints: List[Int]) =
    foldRight(ints, 0)((x, y) => x + y)

  def product (ds: List[Double]): Double =
    ds match { // pattern matching for the parameter ds
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  def product2(ds: List[Double]) =
    foldRight(ds, 1.0)(_ * _) // _ * _ is more concise notation for (x,y) => x * y

  /**
    * This definition only copies values until the first list is exhausted
    * Runtime and Memory: O(length of a1), Cons(a1[0], a1[1] ......, a1[n-1], a2)
    *
    * If we use two arrays, we need to copy all elements in both arrays into the result.
    * */
  def append[A] (a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(x, xs) => Cons(x, append(xs, a2)) // Cons(T, List[T])
    }

  /**
    * Variadic function: (Type _*)
    *
    * It provides a little syntactic sugar for creating and passing a Seq of elements explicitly.
    * Seq is the interface in Scala collections library such as lists, queues and vectors.
    * The argument 'as' will be bound to a Seq[A].
    *
    * Seq is a subtrait of collection.Seq which represents sequences that are guaranteed immutable
    * Sequences are special cases of iterable collections of class Iterable.
    * Unlike Iterable, Sequence always has a defined order of elements.
    * Sequence provides a method apply for indexing.
    * */
  def apply[A] (as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  /** Exercise 3.1 */
  val a = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }


  /** Exercise 3.2 */
  def tail[A] (l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }


  /** Exercise 3.3 */
  def setHead[A] (l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }


  /** Exercise 3.4 */
  def drop[A] (l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }


  /** Exercise 3.5 group and order function arguments to maximize type inference */
  def dropWhile[A] (l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f) // add if condition after pattern
      case _ => l
    }


  /** Exercise 3.6 */
  def init[A] (l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }


  /** Exercise 3.9 */
  def length[A] (as: List[A]): Int =
    foldRight(as, 0)((_, y) => 1 + y)

  /**
    * Exercise 3.13
    *
    * foldLeft() with Tail-recursive
    *
    *      L -> R         L <- R
    *     foldLeft      foldRight
    *        f              f
    *       / \            / \
    *      f  3           1  f
    *     / \               / \
    *    f  2              2  f
    *   / \                  / \
    * acc 1                 3  acc
    *
    * */
  @annotation.tailrec
  def foldLeft[A, B] (l: List[A], acc: B)(f: (B, A) => B): B =
    l match {
      case Nil => acc // return accumulator when the list is empty
      case Cons(h, t) => foldLeft(t, f(acc, h))(f)
    }


  /** Non Tail-recursive */
  def foldRight[A, B] (l: List[A], acc: B)(f: (A, B) => B): B =
    l match {
      case Nil => acc // return accumulator when the list is empty
      case Cons(h, t) => f(h, foldRight(t, acc)(f))
    }


  /**
    * The implementation of 'foldRight' in terms of 'reverse' and 'foldLeft'
    * is common trick ofr avoiding stack overflows when implementing a strict 'foldRight'
    * */
  def foldRightViaFoldLeft[A, B] (l: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), acc)((b, a) => f(a, b))

  def reverse[A] (l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))


  /** Exercise 3.14 */
  def appendUsingFoldLeft[A] (l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2)((acc, h) => Cons(h, acc))
  def appendUsingFoldRight[A] (l1: List[A], l2: List[A]): List[A] =
    foldRightViaFoldLeft(l1, l2)((t, acc) => Cons(t, acc))


  /** Exercise 3.15 */
  def concat[A] (l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())(append)


  /** Exercise 3.16 */
  def addOneToEach (l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, Nil: List[Int])((t, acc) => Cons(t + 1, acc))


  /** Exercise 3.17 */
  def IntToString (l: List[Int]): List[String] =
    foldRightViaFoldLeft(l, Nil: List[String])((t, acc) => Cons(t.toString, acc))
  def DoubleToString (l: List[Int]): List[String] =
    foldRightViaFoldLeft(l, Nil: List[String])((t, acc) => Cons(t.toString, acc))


  /** Exercise 3.18 */
  def map[A, B] (l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((t, acc) => Cons(f(t), acc))


  /** Exercise 3.19 */
  def filter[A] (l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])((t, acc) => if (f(t)) Cons(t, acc) else acc)


  /** Exercise 3.20 */
  def flatMap[A, B] (l: List[A])(f: A => List[B]): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((t, acc) => append(f(t), acc))


  /** Exercise 3.21 */
  def filterViaFlatMap[A] (l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((x) => if (f(x)) List(x) else Nil)


  /** Exercise 3.22 */
  def addPairwise (l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
    }


  /** Exercise 3.23 */
  def zipWith[A, B, C] (l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  /** Exercise 3.24 */
  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean =
    sub match {
      case Nil => true
      case Cons(x, _) => checkSequence(dropWhile(sup)((h) => h != x), sub)
    }

  @annotation.tailrec
  def checkSequence[A] (sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) if x == y => checkSequence(xs, ys)
      case _ => false
    }

  def take[A] (l: List[A], n: Int): List[A] =
    if (n < 0) Nil
    else l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x, take(xs, n - 1))
    }

  def takeWhile[A] (l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, takeWhile(xs)(f)) else Nil
    }

  @annotation.tailrec
  def forall[A] (l: List[A])(f: A => Boolean): Boolean =
    l match {
      case Nil => true
      case Cons(x, xs) => f(x) && forall(xs)(f)
    }

  @annotation.tailrec
  def exists[A] (l: List[A])(f: A => Boolean): Boolean =
    l match {
      case Nil => false
      case Cons(x, xs) => f(x) || exists(xs)(f)
    }
}
