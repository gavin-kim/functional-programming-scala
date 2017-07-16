package C2_GettingStarted // _ bring all members of Listing2_1

/**
  * object keyword creates a new singleton type
  * (similar to anonymous class in Java)
  *
  * Scala doesn't have static keyword. object is often used like static class.
  * val: final variable
  * var: variable
  *
  * In Scala every method has to return some value
  *
  * Tail recursive: a method returns a value and nothing to do after return.
  *                 On the other hand, 1 + go(n - 1, n * acc) is not tail
  *                 position, since the method still have work when it returns.
  *                 Scala automatically compiles a tail position recursion to
  *                 iterative loops that don't consume call stack frames.
  *
  * Function Literal or Anonymous function: [Syntax] (x: Int) => x == 9
  */
object GettingStarted {

  def factorial(n: Int): Int = {
    @annotation.tailrec // compiler check if the method is in tail position.
    def loop(n: Int, acc: Int): Int = { // inner function
      if (n <= 0) acc
      else loop(n - 1, n * acc) // tail position (nothing to do after loop() return)
    }
    loop(n, 1)
  }

  def abs(n: Int): Int = // No need curly braces for one statement
    if (n < 0) -n        // using ; or a newline to separate statements
    else n

  // Higher Order Function (HOF) that takes another function
  def formatResult(f: Int => Int, name: String, n: Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // Monomorphic function: only for one type
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  // Polymorphic(generic) function: for any type
  def findFirst[A](arr: Array[A], func: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= arr.length) -1
      else if (func(arr(n))) n // If the func matches the current element, returns its index
      else loop(n + 1)
    }
    loop(0)
  }

  // HOF takes an anonymous function
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  private def formatAbs(x: Int) = { // able to omit the return type for private
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x)) // String.format. Scala returns the last statement
  }

  def main(args: Array[String]): Unit = { // Unit is used like void in Java
    var arr = Array(1, 2, 3, 45, 5, 3)

    println(findFirst[Int](arr, (x: Int) => x == 3))
  }

}
