package C3_FunctionalDataStructure

/**
  * A pure function must note change data.
  * Therefore, functional data structures are by definition immutable.
  */
object Exercise {
  def main(args: Array[String]): Unit = {

    val msg = "Exercise %s: %s"
    val l1 = List[Int](1, 2, 3, 4, 5)
    val l2 = List[Int](6, 7, 8, 9, 10)
    val l3 = List[Int](11, 12, 13, 14, 15)

    val tree =
      Branch(
        Branch(
          Branch(Leaf(1), Leaf(2)), Branch(
                                      Branch(
                                        Leaf(3), Leaf(4)), Leaf(5))), Leaf(6))

    // Exercise 3.1
    println(msg.format("3.1", List.a))

    // Exercise 3.2
    println(msg.format("3.2", List.tail(l1)))

    // Exercise 3.3
    println(msg.format("3.3", List.setHead(l1, 100)))

    // Exercise 3.4
    println(msg.format("3.4", List.drop(l1, 4)))

    // Exercise 3.5
    println(msg.format("3.5", List.dropWhile(l1)(x => x < 4)))

    // Exercise 3.6
    println(msg.format("3.6", List.init(l1)))

    // Exercise 3.8
    println(msg.format("3.8", List.foldRight(l1, Nil: List[Int])(Cons(_, _))))

    // Exercise 3.9
    println(msg.format("3.9", List.length(l2)))

    // Exercise 3.13
    println(msg.format("3.13", List.reverse(l2)))

    // Exercise 3.14
    println(msg.format("3.14", List.appendUsingFoldLeft(l1, l2)))
    println(msg.format("3.14", List.appendUsingFoldRight(l1, l2)))

    // Exercise 3.15
    println(msg.format("3.15", List.concat(List(l1, l2, l3))))

    // Exercise 3.16
    println(msg.format("3.16", List.addOneToEach(l1)))

    // Exercise 3.17
    println(msg.format("3.17", List.IntToString(l1)))

    // Exercise 3.18
    println(msg.format("3.18", List.map(l1)((x) => x * x)))

    // Exercise 3.19
    println(msg.format("3.19", List.filter(l1)((x) => (x & 1) == 1)))

    // Exercise 3.20
    println(msg.format("3.20", List.flatMap(l1)((x) => List(x, x + 1, x + 2))))

    // Exercise 3.21
    println(msg.format("3.21", List.filter(l2)((x) => (x & 1) == 1)))

    // Exercise 3.22
    println(msg.format("3.22", List.addPairwise(l1, l3)))

    // Exercise 3.23
    println(msg.format("3.23", List.zipWith(l1, l2)((x, y) => x * y)))

    // Exercise 3.24
    println(msg.format("3.24", List.hasSubsequence(l1, List(2, 4))))
    println(msg.format("3.24", List.hasSubsequence(l1, List(1, 2))))
    println(msg.format("3.24", List.hasSubsequence(l1, List(2, 3, 4))))
    println(msg.format("3.24", List.hasSubsequence(l1, List())))

    // take()
    println(msg.format("take()", List.take(l1, 4)))

    // takeWhile()
    println(msg.format("takeWhile()", List.takeWhile(l1)((x) => x < 3)))

    // forall()
    println(msg.format("forall() false", List.forall(l2)((x) => x < 10)))
    println(msg.format("forall() true", List.forall(l2)((x) => x < 11)))

    // exist()
    println(msg.format("exists() true", List.exists(l2)((x) => x == 10)))
    println(msg.format("exists() false", List.exists(l2)((x) => x == 11)))


    // Exercise 3.25
    println(msg.format("3.25", Tree.size(tree)))

    // Exercise 3.26
    println(msg.format("3.26", Tree.maximum(tree)))

    // Exercise 3.27
    println(msg.format("3.27", Tree.depth(tree)))

    // Exercise 3.28

  }
}
