package org.deusaquilus

import scala.collection.MapView
import scala.collection.mutable;
import scala.quoted._
import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
import scala.deriving._

object DerivedImmutable {

  enum WriteOutput:
    def value: Any
    case Leaf(value: Any) extends WriteOutput
    case Node(value: Map[String, Any]) extends WriteOutput

  // sealed trait WriteOutput
  // object WriteOutput {
  //   case class Leaf(value: Any) extends WriteOutput
  //   case class Node(value: Map[String, Any]) extends WriteOutput
  // }

  // object WriteOutput {
  //   class Leaf(val value: Any) extends WriteOutput
  //   object Leaf {
  //     def unapply(leaf: Leaf): Option[Any] = Some(leaf.value)
  //   }
  //   class Node(val value: Map[String, Any]) extends WriteOutput
  //   object Node {
  //     def unapply(node: Node): Option[Map[String, Any]] = Some(node.value)
  //   }
  // }

  import WriteOutput._

  inline given writeArbitraryToMap[T]: WriteToMap[T] = WriteToMap.derived

  object WriteToMapOps {
    extension [T](value: T)(using wtm: WriteToMap[T]) // <: Product
      def writeToMap: Map[String, Any] =
        wtm.writeToMap(value) match
          case Node(map) => map
          case _ => throw new IllegalArgumentException("Not a product type")
  }

  trait WriteToMap[T]:
    def writeToMap(value: T): WriteOutput

  object WriteToMap {

    inline def summonWriter[T]: WriteToMap[T] =
      summonInline[WriteToMap[T]]

    given writeInt: WriteToMap[Int] with
      def writeToMap(value: Int) = WriteOutput.Leaf(value)

    given writeString: WriteToMap[String] with
      def writeToMap(value: String) = WriteOutput.Leaf(value)

    given writeList[T](using WriteToMap[T]): WriteToMap[List[T]] with
      def writeToMap(value: List[T]) = WriteOutput.Leaf(value)

    inline def recurseSum[Types <: Tuple, T](element: T): WriteOutput =
      inline erasedValue[Types] match
        case _: (tpe *: types) =>
          if (element.isInstanceOf[tpe])
            summonWriter[tpe].writeToMap(element.asInstanceOf[tpe])
          else
            recurseSum[types, T](element)
        case _: EmptyTuple =>
          throw new IllegalArgumentException(s"Invalid coproduct type") //${summonInline[Type[T]]}
          // Does not work with Sum Mirrors
          //throw new IllegalArgumentException(s"Invalid coproduct type ${summonInline[Type[T]]}")
    
    inline def recurse[Names <: Tuple, Types <: Tuple](element: Product)(index: Int): Map[String, Any] =
      inline erasedValue[(Names, Types)] match
        case (_: (name *: names), _: (tpe *: types)) =>
          val key = constValue[name].toString
          val value = element.productElement(index).asInstanceOf[tpe]
          val writtenValue = summonWriter[tpe].writeToMap(value).value
          // val writtenValue = 
          //   summonWriter[tpe].writeToMap(value) match
          //     case Node(value) => value
          //     case Leaf(value) => value
          recurse[names, types](element)(index + 1) + (key -> writtenValue)
        case (_: EmptyTuple, _) =>
          Map.empty[String, Any]

    /** if you change to 'inline given' here, you will get the following error in UseDerivedImmutable:
     * {{
     * value toMap is not a member of Person.
     * An extension method was tried, but could not be fully constructed:
     * 
     *     org.deusaquilus.DerivedImmutable.WriteToMapOps.toMap[Person](p)(
     *       ambiguous: both method writeArbitraryToMap in object DerivedImmutable and method derived in object WriteToMap match type org.deusaquilus.DerivedImmutable.WriteToMap[Person]
     *         summon[org.deusaquilus.DerivedImmutable.WriteToMap[Person]]
     *     )
     * }}
     */

    inline def derived[T]: WriteToMap[T] =
      inline summonInline[Mirror.Of[T]] match
        case proMir: Mirror.ProductOf[T] =>
          /*PrintMacPass(*/
          new WriteToMap[T]:
            def writeToMap(value: T): WriteOutput =
              Node(recurse[proMir.MirroredElemLabels, proMir.MirroredElemTypes](value.asInstanceOf[Product])(0))
          /*)*/
        case sumMir: Mirror.SumOf[T] =>
          new WriteToMap[T]:
            def writeToMap(value: T): WriteOutput = recurseSum[sumMir.MirroredElemTypes, T](value)
        case _ =>
          // Does not work with sum mirrors
          //throw new IllegalArgumentException(s"No mirror found for ${summonInline[Type[T]]}")
          throw new IllegalArgumentException(s"No mirror found") // ${summonInline[Type[T]]}
  }
}
