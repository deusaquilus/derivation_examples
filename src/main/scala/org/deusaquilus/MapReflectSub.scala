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
  import Util._

  def reflectCaseClassFields[T <: Product](p: T) = {
    val fieldNames = p.getClass.getDeclaredFields.map(_.getName).toSet
    p.getClass.getDeclaredMethods.filter(m => fieldNames.contains(m.getName)).toList
  }

  def main(args: Array[String]): Unit = {
  }

  def funDerive() = {

  }

  def funMac() = {
  }

  def funA() = {
  }
}

