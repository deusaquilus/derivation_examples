package org.deusaquilus

import scala.collection.MapView
import scala.collection.mutable;
import scala.quoted._
import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
import scala.deriving._

object DerivedUsingUnion {
  sealed trait JustReturn
  object JustReturn extends JustReturn

  trait WriteToMap[T]:
    def writeToMap(mapOrReturn: mutable.Map[String, Any], justReturn: Boolean)(key: String, value: T): Any
    
  object WriteToMapOps {
    extension [T](value: T)(using wtm: WriteToMap[T])
      def writeToMap: MapView[String, Any] =
        val map = mutable.Map[String, Any]()
        wtm.writeToMap(map, false)("", value)
        map.view
  }

  object WriteToMap {
    import WriteToMapOps._

    inline def summonWriter[T]: WriteToMap[T] =
      summonInline[WriteToMap[T]]

    given writeInt: WriteToMap[Int] with
      def writeToMap(map: mutable.Map[String, Any], justReturn: Boolean)(key: String, value: Int): Any = 
        if (justReturn) value
        else map.put(key, value); map

    given writeString: WriteToMap[String] with
      def writeToMap(map: mutable.Map[String, Any], justReturn: Boolean)(key: String, value: String): Any =
        if (justReturn) value
        else map.put(key, value); map
    
    inline def recurse[Names <: Tuple, Types <: Tuple](element: Product, map: mutable.Map[String, Any])(index: Int): Unit =
      inline erasedValue[(Names, Types)] match
        case (_: (name *: names), _: (tpe *: types)) =>
          val key = constValue[name].toString
          val value = element.productElement(index).asInstanceOf[tpe]
          summonWriter[tpe].writeToMap(map, false)(key, value)
          recurse[names, types](element, map)(index + 1)
        case (_: EmptyTuple, _) =>
          // Ignore

    inline def derived[T] =
      inline summonInline[Mirror.ProductOf[T]] match
        case proMir: Mirror.ProductOf[T] =>
          new WriteToMap[T] {
            def writeToMap(map: mutable.Map[String, Any], justReturn: Boolean)(key: String, value: T): Any =
              if (justReturn)
                val newMap = mutable.Map[String, Any]()
                recurse[proMir.MirroredElemLabels, proMir.MirroredElemTypes](value.asInstanceOf[Product], newMap)(0)
                newMap
              else
                recurse[proMir.MirroredElemLabels, proMir.MirroredElemTypes](value.asInstanceOf[Product], map)(0)
                map
          }
        case _ =>
          throw new IllegalArgumentException(s"No mirror found for ${summonInline[Type[T]]}")
    
  }
}