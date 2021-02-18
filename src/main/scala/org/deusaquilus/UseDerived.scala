package org.deusaquilus

object UseDerived {
  import Derived._

  def main(args: Array[String]): Unit = {

  }

  def simpleCaseClassWithDerive(): Unit = {
    case class Person(firstName: String, lastName: String, age: Int) derives WriteToMap
  }
}