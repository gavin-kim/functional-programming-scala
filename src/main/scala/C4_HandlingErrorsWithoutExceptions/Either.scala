package C4_HandlingErrorsWithoutExceptions


/**
  * Either has only 2 cases, The both cases carry a value
  *
  * Left is used for the failure case (E type for Error)
  * Right is used for the success case (A type for some value)
  */

sealed trait Either[+E, +A] {

  def map[B] (f: A => B): Either[E, B] =
    this match {
      case Left(x) => Left(x)
      case Right(x) => Right(f(x))
    }

  def flatMap[EE >: E, B] (f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(x) => Left(x)
      case Right(x) => f(x)
    }

  def orElse[EE >: E, B >: A] (b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case _ => this
    }

  def map2[EE >: E, B, C] (b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b.map(b => f(a, b))) // b.map() return Right(f(a, b)): Either[E, A]
}
case class Left[+E] (value: E) extends Either[E, Nothing]
case class Right[+A] (value: A) extends Either[Nothing, A]

object Either {

  /** return String message in case of failure */
  def mean (xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs. sum / xs.length)


  /** return Exception in case of failure */
  def safeDiv (x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  /** To convert from an Exception to an Either with lazy argument (a: => A) */
  def Try[A] (a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /** Exercise 4.7 **/
  def sequence[E, A] (l: List[Either[E, A]]): Either[E, List[A]] =
    l match {
      case Nil => Right(Nil)
      case eit :: eits => eit flatMap (a => sequence(eits) map (a :: _))
    }

  def traverse[E, A, B] (l: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    l match {
      case Nil => Right(Nil)
      case eit :: eits => f(eit) flatMap (a => traverse(eits)(f) map (a :: _))
  }

}
