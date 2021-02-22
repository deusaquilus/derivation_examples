package org.deusaquilus

import scala.collection.mutable

// Works
// sealed trait Name
// object Name {
//   case class Simple(first: String, last: String) extends Name
// }

// Works
enum Name:
  case Simple(first: String, last: String) extends Name
  case Title(first: String, middle: String, last: String) extends Name

object UseDerivedImmutable {
  import DerivedImmutable._
  import WriteToMapOps._

  def main(args: Array[String]): Unit = {
    //println(simpleCaseClassWithDerive)
    // println(derivedWithListLeaf)
    // println(derivedWithListNode)
    // println(derivedWithListAddr)
    deriveCoproduct()
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

  def deriveCoproduct() = {
    // Inline enum blows up horribly (make sure to remove the summonInline[Type[T]] to see that)
    // enum Name:
    //   case Simple(first: String, last: String) extends Name
    //   case Title(first: String, middle: String, last: String) extends Name

    case class Person(name: Name, age: Int)
    val p1 = Person(Name.Simple("Joe", "Bloggs"), 123)
    //val n = Name.Simple("Joe", "Bloggs")

    //given WriteToMap[Name] = WriteToMap.derived
    
    println(p1.writeToMap)

    // val p2 = Person(Name.Title("Joe", "P.", "Bloggs"), 123)
    // println(p1.writeToMap)
  }
}