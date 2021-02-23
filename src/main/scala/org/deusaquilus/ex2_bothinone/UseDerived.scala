package org.deusaquilus

import scala.collection.mutable

object UseDerived {
  import Derived._
  import WriteToMapOps._

  def main(args: Array[String]): Unit = {
    simpleCaseClassWithDerive()
  }

  def simpleCaseClassWithDerive(): Unit = {
    case class Person(firstName: String, lastName: String, age: Int) derives WriteToMap
    val p = Person("Joe", "Bloggs", 123)
    val map = p.writeToMap

    println(p.writeToMap.toMap)
  }

  // def simpleCaseClassWithDeriveGiven(): Unit = {
  //   case class Person(firstName: String, lastName: String, age: Int)
  //   val p = Person("Joe", "Bloggs", 123)
  //   PrintMac(p.writeToMap) //helloooo
  //   val map = p.writeToMap

  //   println(p.writeToMap.toMap)
  // }

  def derivedWithNested(): Unit = {
      case class Name(first: String, last: String) derives WriteToMap
      case class Person(name: Name, age: Int) derives WriteToMap
      val p = Person(Name("Joe", "Bloggs"), 123)
      val map = p.writeToMap
      println(map.toMap)
  }

  def derivedWithExtension(): Unit = {
    given writeLong: WriteToMap[Long] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: Long): Unit = map.put(key, value)

    case class Person(firstName: String, lastName: String, age: Long) derives WriteToMap
    val q = Person("Q", "Q", 4000000000L)
    val map = q.writeToMap
    println(map.toMap)
  }

  def derivedWithExtensionCustomInt(): Unit = {
    case class ConvertableAge(value: Int):
      def numDecades = value/10

    given writeLong: WriteToMap[ConvertableAge] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, ca: ConvertableAge): Unit = map.put(key, ca.value)

    case class Person(firstName: String, lastName: String, age: ConvertableAge) derives WriteToMap
    val q = Person("Joe", "Bloggs", ConvertableAge(123))
    val map = q.writeToMap
    println(map.toMap)
  }
}