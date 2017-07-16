package C5_StrictnessAndLaziness

import scala.collection.immutable.Stream.cons

/**
  * Strict function:     Always evaluates its arguments
  * Non-strict function: Chooses not to evaluate one or more of its arguments
  *
  * thunk: the unevaluated form of an expression
  *        we can invoke thunk to get a result by passing an empty argument onTrue() or onFalse()
  *
  * lazy keyword to val declaration will delay evaluation of the right-hand side
  * until it's first referenced. It will also cache the result.
  *
  *
  * Infinite stream
  * val ones: Stream[Int] = Stream.cons(1, ones)
  *
  * Although ones is infinite, stream functions generate the demanded output
  * println(ones.take(5).toList)
  * println(ones.exists(_  % 2 != 0))
  *
  */
object StrictAndNonStrict {

  def main(args: Array[String]): Unit = {

    //println(StrictAndNonStrict.evalTwice(true, { println("hi"); 1 + 41} ))
    //println(StrictAndNonStrict.evalOnceWithLazy(true, { println("hi"); 1 + 41} ))
    var stream = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
    var stream1 = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
    var substream = Stream[Int](4, 5, 6, 7)
    var stream2 = Stream[Int](10, 11, 12, 13, 14, 15)
    var stream3 = Stream[Int](16, 17, 18, 19)
    var streams = Stream[Stream[Int]](stream, stream2, stream3)

    println(stream)

    println("\nExercise 5.1")
    println(stream.drop(3).toList)

    println("\nExercise 5.2")
    println(stream.take(5).toList)
    println(stream.drop(5).toList)

    println("\nExercise 5.3")
    println(stream.takeWhile(value => value < 3).toList)

    println("\nExercise 5.4")
    println(stream.takeWhile(value => value < 3).toList)

    println("\nExercise 5.5")
    println(stream.takeWhile2(value => value == 2).toList)

    println("\nExercise 5.6")
    stream.foldRight(0)((h, acc) => {
      println(h)
      acc
    })

    println("\nExercise 5.7 map")
    println(stream.map(v => v * v).toList)

    println("\nExercise 5.7 filter")
    println(stream.filter(v => v % 2 == 0).toList)

    println("\nExercise 5.7 append")
    println(stream.append(stream2).toList)

    println("\nExercise 5.7 flatMap")
    println(streams.flatMap(x => x.map(a => a * a)).toList)

    println("\nExercise 5.8")
    println(Stream.constant(1))

    //println("\nExercise 5.9")
    //println(Stream.from(10).filter(v => v < 100).forEach(x => print(x)))

    //println("\nExercise 5.10")
    //println(Stream.fib().filter(value => value < 100).forEach(x => print(x)))

    println("\nExercise 5.13 (mapViaUnfold)")
    println(stream.mapViaUnfold(x => x * x).toList)

    println("\nExercise 5.13 (takeViaUnfold)")
    println(stream.takeViaUnfold(2).toList)

    println("\nExercise 5.13 (takeWhileViaUnfold)")
    println(stream.takeWhileViaUnfold(x => x < 3).toList)

    println("\nExercise 5.13 (zipWithViaUnfold)")
    println(stream.zipWith(stream2)((a, b) => a * b).toList)

    println("\nExercise 5.13 (zipAllViaUnfold)")
    println(stream.zipAll(stream2).toList)

    println("\nExercise Test folderLeft ")
    println(stream.foldLeft("")((acc, x) => x + acc))

    println("\nExercise Test folderRight ")
    println(stream.foldRight("")((x, acc) => x + acc))

    println("\nExercise Test dropWhile ")
    println(stream.dropWhile(x => x < 7).toList)

    println("\nExercise Test hasSubsequence ")
    println(stream.hasSubsequence(stream))


    println("\nExercise 5.14 startWith ")
    println(stream.startWith(stream))


    println("\nExercise 5.15 Test tails ")
    stream.tails.forEach(s => println(s.toList))

    println("\nExercise 5.16 Test tails ")
    stream.scanRight(0)(_ + _).forEach(s => println(s))

  }

  /**
    * Non-strict function with function argument( () => A  )
    * () => A   is a function that accepts zero arguments and returns A type
    * By default, Scala doesn't cache the result of evaluating an arguments
    * */
  def if2[A] (bool: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (bool) onTrue() else onFalse()

  /**
    * Non-strict function with lazy argument ( => A )
    * i is referenced twice in the body of if3
    * It's evaluated each time by passing the block
    */
  def evalTwice[A] (bool: Boolean, i: => Int) =
    if (bool) i + i else 0 // i is referenced and evaluated twice

  /**
    * Cache the value explicitly using lazy keyword to evaluate the result only once.
    */
  def evalOnceWithLazy (bool: Boolean, i: => Int) = {
    lazy val j = i // lazy keyword delays evaluation (j = i) until j is first referenced
    if (bool) j + j else 0 // j is referenced here, (j = i) is evaluated this time
  }
}
