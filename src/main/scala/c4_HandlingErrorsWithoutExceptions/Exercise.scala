package c4_HandlingErrorsWithoutExceptions

/**
  * Sentinel value (using specific value) VS  Option type (using Wrapper)
  *
  * function:     Int => Int                 function:   Int   => Option[Int]
  * function(valid)   => Int                 function(valid)   => Some(Int)
  * function(invalid) => -99999              function(invalid) => None
  */

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)


object Exercise {

  def main(args: Array[String]): Unit = {
    println(mkPerson("f", -32))
  }

  def mkName(name: String): Either[String, Name] =
    if (name == null || name.isEmpty)
      Left("Name is empty")
    else
      Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0)
      Left("Age is out of range.")
    else
      Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)


  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("Fail!")
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("Fail!")): Int) // A thrown Exception can be given any type
    } catch {
      case e: Exception => 100
    }
  }

}
