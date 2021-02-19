package org.deusaquilus

import scala.collection.mutable

object UseDerivedMirrorSummon {
  import DerivedMirrorSummon._
  import WriteToMapOps._

  def main(args: Array[String]): Unit = {
    simpleCaseClassWithDeriveGiven()
  }

  def simpleCaseClassWithDeriveGiven(): Unit = {
    inline given writeArbitraryToMap[T]: WriteToMap[T] = WriteToMap.derived

    case class Person(firstName: String, lastName: String, age: Int) // derives WriteToMap
    val p = Person("Joe", "Bloggs", 123)
    PrintMac(p.writeToMap)
    val map = p.writeToMap

    println(p.writeToMap.toMap)
  }
}