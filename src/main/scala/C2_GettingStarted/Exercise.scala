package C2_GettingStarted

object Exercise {

  // Exercise 2.1
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev:Int, curr: Int): Int = {
      if (n <= 0) prev
      else if (n == 1) curr
      else loop(n - 1, curr, prev + curr)
    }
    loop(n, 0, 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], isOrdered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if (isOrdered(as(n), as(n + 1))) loop(n + 1)
      else false
    }
    loop(0)
  }

  // Exercise 2.3 (partially apply)
  def curry[A, B, C] (f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b) // curry: return partial function (B => C)

  // Exercise 2.4
  def uncurry[A, B, C] (f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C] (f: B => C, g: A => B) : A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    var ordered = Array(1 ,2, 3, 4, 5)
    var unordered = Array(1 ,2, 3, 4, 5, 4)

    println(isSorted[Int](ordered, (a, b) => a <= b))
    println(isSorted[Int](unordered, (a, b) => a <= b))
  }
}
