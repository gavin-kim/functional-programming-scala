package C5_StrictnessAndLaziness

import Stream._

import scala.annotation.tailrec
/**
  * head and tail are non-strict in Cons
  */
sealed trait Stream[+A] {

  /** Exercise 5.1 */
  def toList: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List()
    }

  /** Exercise 5.2 */
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Cons[A](h, () => t().take(n - 1))
      case _ => Empty
    }

  /** Exercise 5.3 */
  def takeWhile(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if f(h()) => Cons[A](h, () => t().takeWhile(f))
      case _ => Empty
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1);
      case _ => this
    }

  def dropWhile(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) => if (f(h())) t().dropWhile(f) else this
      case _ => empty
    }


  /** Exercise 5.4 */
  def forAll(f: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => f(h()) && t().forAll(f)
      case _ => true
    }

  /** Exercise 5.5 */
  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, acc) =>
      if (f(h))
        cons[A](h, acc)
      else
        acc
    )

  /** Exercise 5.6 */
  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, t) => Some(h()) // explicit forcing of the h thunk using h()
    }

  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /** Exercise 5.7 */
  def map[B](f: A => B): Stream[B] =
    this match {
      case Cons(h, t) => cons[B](f(h()), t().map(f))
      case Empty => empty
    }

  def map2[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] =
    (this, stream) match {
      case (Cons(a, as), Cons(b, bs)) => cons(f(a(), b()), as().map2(bs())(f))
      case _ => empty
    }

  def append[B >: A](stream: => Stream[B]): Stream[B] =
    foldRight(stream)((h, acc) => cons[B](h, acc))

  // flatMap get function (each item => stream) then stream[stream] to flat stream
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, acc) => f(h).append(acc))

  def filter(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) => if (f(h())) cons[A](h(), t().filter(f)) else t().filter(f)
      case Empty => empty
    }

  /** Exercise 5.13 via unfold */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some(f(h()), t()) // (next value, next state)
      case _ => None                       // to terminate
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (v, Cons(h, t)) if v > 0 => Some(h(), (v - 1, t()))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, stream) { // pattern: value, state
      case (Cons(a, as), Cons(b, bs)) => Some(f(a(), b()), (as(), bs()))
      case _ => None
    }

  def zipAll[B](stream: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, stream) { // pattern: S => Option(A, S)
      case (Empty, Empty) => None
      case (Cons(a, as), Empty) => Some((Some(a()), None), (as(), Empty))
      case (Empty, Cons(b, bs)) => Some((None, Some(b())), (Empty, bs()))
      case (Cons(a, as), Cons(b, bs)) => Some((Some(a()), Some(b())), (as(), bs()))
    }

  /** Even though filter transforms the whole stream,
    * that transformation is done lazily, so find terminates as soon as it's found */
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  /**
    * tail recursive version
    */
  def toList2: List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] =
      stream match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }
    go(this, List()).reverse
  }

  /**
    * to avoid using reverse()
    */
  def toListFast: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A] // using mutable temporary list

    @annotation.tailrec
    def go(stream: Stream[A]): List[A] =
      stream match {
        case Cons(h, t) =>
          buffer += h()
          go(t())
        case _ => buffer.toList
      }
    go(this)
  }

  def exists(f: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => f(h()) || t().exists(f)
      case _ => false
    }

  /**
    * def foldRight[B] (acc: B)(f: (A, B) => B): B
    *
    * ( => B ) means that the function f takes (A, => B) and
    * may not to evaluate ( => B )
    *
    *
    * *evaluates f(a) first b is evaluated depending on f(a)
    *
    * def exists(f: A => Boolean): Boolean =
    *   foldRight(false)((a, b) => f(a) || b)
    *
    */
  def foldRight[B] (acc: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(acc)(f)) // evaluates t().foldRight(acc)(f) lazily
      case _ => acc
    }

  def foldLeft[B](acc: => B)(f: (B, => A) => B): B =
    this match {
      case Cons(h, t) => t().foldLeft(f(acc, h()))(f)
      case _ => acc
    }

  def forEach(f: A => Unit): Unit =
    this match {
      case Cons(h, t) => f(h()); t().forEach(f)
      case _ => Empty
  }

  def hasSubsequence[B >: A](sub: Stream[B]): Boolean =
    sub match {
      case Cons(h, _) => this.dropWhile(x => x != h()).startWith(sub)
      case _ => false
    }

  def startWith[B >: A](stream: Stream[B]): Boolean = {
    zipAll(stream).takeWhile(t => t._2.isDefined).forAll {
      case (a, b) => a.get == b.get
    }
  }

  /** Exercise 5.15
    * Stream(1,2,3) => Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()) */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case stream => Some(stream, stream.drop(1))
    } append Stream(Empty)

  /** Exercise 5.16 */
  def scanRight[B](acc: B)(f: (A, => B) => B): Stream[B] =
    // tuple accumulate
    foldRight((acc, Stream(acc)))((a, state) => {
      // s is passed by-name and used in by-name args in f and cons.
      // so use lazy val to ensure only on evaluation
      lazy val s = state
      val sum = f(a, s._1)
      (sum, cons(sum, s._2))
    })._2

  def reverse: Stream[A] =
    foldLeft(empty[A])((acc, a) => cons(a, acc))
}
case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  /** smart constructor, for constructing a data type
    * By convention, it typically lowercase the first letter of the corresponding data constructor.
    * This function takes care of memoizing the by-name arguments for the head an tail of the Cons.
    * */
  def cons[A] (h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h // cache head and tail as lazy values to avoid repeated evaluations
    lazy val tail = t
    Cons(() => head, () => tail) // construct takes explicit thunks (() => head) and (() => tail)
  }

  /** constructor for creating an empty stream of a particular type */
  def empty[A]: Stream[A] = Empty

  /** constructor a Stream form multiple elements, apply() constructor without new keyword */
  def apply[A] (as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*)) // apply(as.tail: _*) won't be evaluated until you force the Stream

  /** Exercise 5.8 (generates an infinite Stream of a */
  def constant[A](a: A): Stream[A] = {
    lazy val infiniteStream: Stream[A] = cons(a, infiniteStream)
    infiniteStream
  }

  /** Exercise 5.9 (generates an infinite stream of integers, starting from n) */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /** Exercise 5.10 (generates an infinite fibonacci) */
  def fib(): Stream[Int] = {

    def go(a: Int, b: Int): Stream[Int] =
        cons(a, go(b, a + b))

    go(0, 1)
  }

  /** Exercise 5.11
    *
    * recursive:   A function consumes data,
    *              terminates by recursing on smaller inputs
    * corecursive: A function produces data,
    *              not terminates so long as it remains productive
    *              productive as long as until f() terminates
    *
    * Option: indicates when the Stream should be terminated
    *
    * state: initial state
    * f: a function produces both the next state and value
    * */
  def unfold[A, S] (state: S)(f: S => Option[(A, S)]): Stream[A] =
    f(state) match {
      case Some((v, s)) => cons(v, unfold(s)(f)) // if the next value and state exist
      case None => empty
    }

  /** Exercise 5.12 */
  def onesViaUnfold(): Stream[Int] =
    unfold(1)(state => Some(1, 1))

  def constantViaUnfold[A] (a: A): Stream[A] =
    unfold(a)(state => Some(a, a))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(state => Some(n, n + 1))

  def fibViaUnfold(): Stream[Int] =
    unfold((0, 1)) { // init state
      case (a, b) => Some(a, (b, a + b)) // Some(next value, next state)
    }


}
