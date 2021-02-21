package org.deusaquilus

import scala.collection.mutable

object UseDerivedImmutable {
  import DerivedImmutable._
  import WriteToMapOps._

  def main(args: Array[String]): Unit = {
    println(simpleCaseClassWithDerive)
    println(derivedWithListLeaf)
    println(derivedWithListNode)
    println(derivedWithListAddr) //hellooooooooooo
  }

  def simpleCaseClassWithDerive = {
    case class Person(firstName: String, lastName: String, age: Int)
    val p = Person("Joe", "Bloggs", 123)
    p.writeToMap
  }

  def derivedWithListLeaf = {
    case class Person(firstName: String, lastName: String, nicknames: List[String])
    val p = Person("Yosef", "Bloggs", List("Joseph", "Joe"))  
    p.writeToMap
  }

  // Does not work
  def derivedWithListNode = {
    case class Address(street: String, zip: Int)
    case class Person(firstName: String, lastName: String, addresses: List[Address])
    val p = 
      Person("Yosef", "Bloggs", List(
        Address("123 Place", 11122), 
        Address("456 Ave", 11122))
      )
    p.writeToMap
  }

  def derivedWithListAddr = {
    case class Address(street: String, zip: Int)
    case class Person(firstName: String, lastName: String, addresses: Address)
    val p = Person("Yosef", "Bloggs", Address("123 Place", 11122))
    p.writeToMap
  }

  // def simpleCaseClassWithDeriveGiven(): Unit = {
  //   case class Person(firstName: String, lastName: String, age: Int)
  //   val p = Person("Joe", "Bloggs", 123)
  //   PrintMac(p.writeToMap) //helloooo
  //   val map = p.writeToMap

  //   println(p.writeToMap.toMap)
  // }

  // def derivedWithExtension(): Unit = {
  //   given writeLong: WriteToMap[Long] with
  //     def writeToMap(map: mutable.Map[String, Any])(key: String, value: Long): Unit = map.put(key, value)

  //   case class Person(firstName: String, lastName: String, age: Long) derives WriteToMap
  //   val q = Person("Q", "Q", 4000000000L)
  //   val map = q.writeToMap
  // }
}