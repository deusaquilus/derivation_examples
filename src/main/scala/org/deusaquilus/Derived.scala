package org.deusaquilus

import scala.collection.MapView
import scala.collection.mutable;
import scala.quoted._
import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
import scala.deriving._

// extension [T](t: T)(using writeToMap: WriteToMap[T])
//     def 

object Derived {

  trait WriteToMap[T]:
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit
    
  object WriteToMapOps {
    extension [T](value: T)(using wtm: WriteToMap[T])
      def writeToMap: MapView[String, Any] =
        val map = mutable.Map[String, Any]()
        wtm.writeToMap(map)("", value)
        map.view
  }

  object WriteToMap {

    inline def summonWriter[T]: WriteToMap[T] =
      summonInline[WriteToMap[T]]

    given writeInt: WriteToMap[Int] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: Int): Unit = map.put(key, value)

    given writeString: WriteToMap[String] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: String): Unit = map.put(key, value)
    
    inline def recurse[Names <: Tuple, Types <: Tuple](element: Product, map: mutable.Map[String, Any])(index: Int): Unit =
      inline erasedValue[(Names, Types)] match
        case (_: (name *: names), _: (tpe *: types)) =>
          val key = constValue[name].toString
          val value = element.productElement(index).asInstanceOf[tpe]
          summonWriter[tpe].writeToMap(map)(key, value)
          recurse[names, types](element, map)(index + 1)
        case (_: EmptyTuple, _) =>
          // Ignore

    // Uncomment PrintMacPass to print contents of this macro on a recompile
    inline def derived[T](using mir: Mirror.Of[T]) = /*PrintMacPass*/(new WriteToMap[T] {
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit =
        inline mir match
          case proMir: Mirror.ProductOf[T] =>
            recurse[proMir.MirroredElemLabels, proMir.MirroredElemTypes](value.asInstanceOf[Product], map)(0)
          case _ =>
            throw new IllegalArgumentException(s"No mirror found for ${value}")
    })
  }
}