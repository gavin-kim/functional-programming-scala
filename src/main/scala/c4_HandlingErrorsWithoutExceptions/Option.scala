package c4_HandlingErrorsWithoutExceptions

import scala.util.Try

/**
  * Throw exceptions only for no recoverable errors
  * Use Option or Either for recoverable errors
  *
  * Option[A] is a different type than A,
  * So the compiler won't let us forget to explicitly handle the possibility of None
  * */
sealed trait Option[+A] {

  /** Apply f if the Option is not None. */
  def map[B] (f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  def map2[B, C] (b: Option[B])(f: (A, B) => C): Option[C] =
    this.flatMap(aa => b.map(bb => f(aa, bb)))

  /** Apply f. which may fail, to the Option if not None (Wrapper function) */
  def flatMap[B] (f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  /**
    * [B >: A] means B must be equal to or a super type of A
    * (default: => B) means default argument is type of B,
    * but It won't be evaluated until it's needed by the function
    * */
  def getOrElse[B >: A] (default: => B): B =
    this match {
      case None => default
      case Some(x) => x
    }

  /** Don't evaluate ob unless needed */
  def orElse[B >: A] (ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case _ => this
    }

  /** Convert Some to None if the value doesn't satisfy f */
  def filter (f: A => Boolean): Option[A] =
    this match {
      case Some(x) if f(x) => Some(x)
      case _ => None
    }

  def lift[A, B] (f: A => B): Option[A] => Option[B] =
    _.map(f)
}

/**
  * Option has 2 cases: Some, None
  */
case object None extends Option[Nothing]
case class Some[+A] (get: A) extends Option[A]


object Option {

  def main(args: Array[String]): Unit = {
    println(variance(List(3, 4, 4, 5, 6, 8)).orElse(None))
    println(parseInsuranceRateQuote("32", "ferg"))
    println(parseInsuranceRateQuote("32", "5"))
  }

  /** Return type reflects the possibility that the result may not always be defined */
  def mean(seq: Seq[Double]): Option[Double] =
    if (seq.isEmpty) None
    else Some(seq.sum / seq.length)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age / 20 * numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String):
  Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    optAge.map2(optTickets)(insuranceRateQuote)
  }

  /** To convert from an Exception to an Option with lazy argument (a: => A) */
  def Try[A] (a: => A): Option[A] =
    try {
      Some(a)
    } catch {
      case _: Exception => None
    }

  /** Exercise 4.2 */
  def variance(seq: Seq[Double]): Option[Double] =
    mean(seq).flatMap(m => mean(seq.map(x => math.pow(x - m, 2))))
  // if mean(seq) return None, It will immediately return None

  /** Exercise 4.4
    * combines a list of Options into one Option containing a list of all Somes
    * All Options are some in the list => Some; otherwise => None
    * */
  def sequence[A] (l: List[Option[A]]): Option[List[A]] =
    l match {
      case Nil => Some(Nil)
      case optA :: optAs => optA.flatMap(a => sequence(optAs).map(a :: _))
      // flatMap returns None or Some    a: A    Option[a :: List[A]]
    }

  def traverse[A, B] (l: List[A])(f: A => Option[B]): Option[List[B]] =
    l match {
      case Nil => Some(Nil)
      case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
    }
}




