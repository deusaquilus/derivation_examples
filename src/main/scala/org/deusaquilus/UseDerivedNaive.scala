package org.deusaquilus

import scala.collection.mutable




object UseDerivedNaive {
  import DerivedNaive._
  import WriteToMapOps._

  def main(args: Array[String]): Unit = {
    personSimpleCase()
  }

  def personSimpleCase(): Unit = {
    case class Person(firstName: String, lastName: String, age: Int) derives WriteToMap
    val p = Person("Joe", "Bloggs", 123)
    PrintMac(p.writeToMap) //helloooo
    val map = p.writeToMap

    println(p.writeToMap.toMap)
  }
}