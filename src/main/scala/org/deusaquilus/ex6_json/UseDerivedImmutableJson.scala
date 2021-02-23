package org.deusaquilus.ex6_json

import scala.collection.mutable

object UseDerivedImmutableJson {
  import DerivedImmutableJson._
  import WriteOutputCodec._

  def main(args: Array[String]): Unit = {
    println("===== Starting ====")
    println(simpleCaseClassWithDerive)
    println("===== Starting Leaf ====")
    println(derivedWithListLeaf)
    println("===== Starting Node ====")
    println(derivedWithListNode)
    println("===== Starting Addr ====")
    println(derivedWithListAddr)
  }

  def simpleCaseClassWithDerive = {
case class Person(firstName: String, lastName: String, age: Int)
val p = Person("Joe", "Bloggs", 123)
p.toJson
  }

  def derivedWithListLeaf = {
    case class Person(firstName: String, lastName: String, nicknames: List[String])
    val p = Person("Yosef", "Bloggs", List("Joseph", "Joe"))  
    p.toJson
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
    p.toJson
  }

  def derivedWithListAddr = {
    case class Address(street: String, zip: Int)
    case class Person(firstName: String, lastName: String, addresses: Address)
    val p = Person("Yosef", "Bloggs", Address("123 Place", 11122))
    p.toJson
  }
}