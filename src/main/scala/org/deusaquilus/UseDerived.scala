package org.deusaquilus

object UseDerived {
  import Derived._

  def main(args: Array[String]): Unit = {
    simpleCaseClassWithDerive()
  }

  def simpleCaseClassWithDerive(): Unit = {
    import DerivedNaive._
    import WriteToMapOps._

    case class Person(firstName: String, lastName: String, age: Int) derives WriteToMap
    val p = Person("Joe", "Bloggs", 123)
    PrintMac(p.writeToMap) //helloooo
    val map = p.writeToMap

    println(p.writeToMap.toMap)
  }
}