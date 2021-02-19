package org.deusaquilus

import scala.collection.mutable;
import scala.collection.MapView
import scala.quoted._
import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
import scala.deriving._

object DerivedNaive {
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
    given writeInt: WriteToMap[Int] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: Int): Unit = map.put(key, value)

    given writeString: WriteToMap[String] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: String): Unit = map.put(key, value)

    inline def recurseNames[Names <: Tuple]: List[String] =
      inline erasedValue[Names] match
        case _: (name *: names) =>
          constValue[name].toString +: recurseNames[names]
        case _: EmptyTuple =>
          Nil

    inline def summonWriter[T]: WriteToMap[T] =
      summonInline[WriteToMap[T]]
    
    inline def recurseTypes[Types <: Tuple]: List[WriteToMap[Any]] =
      inline erasedValue[Types] match
        case _: (tpe *: types) =>
          summonWriter[tpe].asInstanceOf[WriteToMap[Any]] +: recurseTypes[types]
        case _: EmptyTuple =>
          Nil

    // Uncomment PrintMacPass to print contents of this macro on a recompile
    inline def derived[T](using mir: Mirror.Of[T]) = /*PrintMacPass*/(new WriteToMap[T] { //hello
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit =
        inline mir match
          case proMir: Mirror.ProductOf[T] =>
            val names = recurseNames[proMir.MirroredElemLabels]
            val writers = recurseTypes[proMir.MirroredElemTypes]
            names.zip(writers).zipWithIndex.map {
              case ((name, writer), i) => 
                writer.writeToMap(map)(name, value.productIdx(i))
                
            }
          case _ =>
            throw new IllegalArgumentException(s"No mirror found for ${value}")
    })
  }

  extension (product: Any)
    def productIdx(i: Int) = product.asInstanceOf[Product].productElement(i)
}