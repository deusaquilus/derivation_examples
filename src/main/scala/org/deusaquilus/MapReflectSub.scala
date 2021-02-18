package org.deusaquilus

import scala.collection.mutable;
import scala.quoted._
import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
import scala.deriving._

object DerivedSub {
  trait WriteToMap[T]:
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit

  given WriteToMap[Int] with
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: Int): Unit = map.put(key, value)

  given WriteToMap[String] with
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: String): Unit = map.put(key, value)

  type ProductOf[T <: Product]

  inline def smartSummonWriter[T](map: mutable.Map[String, Any])(key: String, value: T): Unit =
    inline erasedValue[T] match
      case _: ProductOf[tpe] => 
        val newMap = mutable.Map[String, Any]()
        summonInline[WriteToMap[T]].writeToMap(newMap)(key, value)
        map.put(key, newMap)
      case _ => 
        summonInline[WriteToMap[T]].writeToMap(map)(key, value)
  
  inline def writeProduct[Names <: Tuple, Types <: Tuple](element: Product, map: mutable.Map[String, Any])(index: Int): Unit =
    inline erasedValue[(Names, Types)] match
      case (_: (name *: names), _: (tpe *: types)) =>
        val key = constValue[name].toString
        val value = element.productElement(index).asInstanceOf[tpe]
        smartSummonWriter[tpe](map)(key, value)
        writeProduct[names, types](element, map)(index + 1)
      case (_: EmptyTuple, _) =>

  inline def derived[T](using mir: Mirror.Of[T]) = new WriteToMap[T] {
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit =
      inline mir match
        case proMir: Mirror.ProductOf[T] =>
          writeProduct[proMir.MirroredElemLabels, proMir.MirroredElemTypes](value.asInstanceOf[Product], map)(0)
        case _ =>
          throw new IllegalArgumentException(s"No mirror found for ${value}")
  }
}

object MainSub {
  

  def simpleSub(): Unit = {
    case class Name(first: String, last: String)
    case class Person(name: Name, age: Int)
    val p = Person(Name("Joe", "Bloggs"), 123)
    val map = mutable.Map[String, Any]()

    def toMapManual(p: Person) =
      Map("name" -> Map("first" -> p.name.first, "last" -> p.name.last), "age" -> p.age)

    PopulateSubMaps.populateMap(p, map)
    println(pprint(map))
    //println(pprint.apply(toMap(p)))
    
  }

  def polyDefSub(): Unit = {
    trait Name
    case class SimpleName(first: String, last: String) extends Name
    case class Title(first: String, middle: String, last: String) extends Name
    case class Person(name: Name, age: Int)
  }

  def userDefSub(): Unit = {
    case class Name(first: String, last: String)
    case class ConvertableAge(value: Int) {
      def numDecades = value/10
    }
    case class Person(name: Name, age: ConvertableAge)

    val p = Person(Name("Joe", "Bloggs"), ConvertableAge(123))
    val map = mutable.Map[String, Any]()
    PopulateSubMaps.populateMap(p, map)

    println(pprint(map))
    //println(pprint.apply(toMap(p)))

    Map(
      "name" -> Map("last" -> "Bloggs", "first" -> "Joe"), 
      "age" -> Map("value" -> 123)
    )
  }

  def main(args: Array[String]): Unit = {
    userDefSub()
  }
  

  

  def funDerive() = {

  }

  def funMac() = {
  }

  def funA() = {
  }
}

