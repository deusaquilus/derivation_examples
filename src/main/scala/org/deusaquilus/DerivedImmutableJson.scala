// package org.deusaquilus

// import scala.collection.MapView
// import scala.collection.mutable;
// import scala.quoted._
// import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
// import scala.deriving._

// object DerivedImmutableJson {

//   enum WriteOutput:
//     case Leaf(value: Any) extends WriteOutput
//     case Node(map: Map[String, WriteOutput]) extends WriteOutput
//     case Arr(list: List[WriteOutput]) extends WriteOutput

//   import WriteOutput._

//   inline given writeArbitraryToMap[T]: WriteToMap[T] = WriteToMap.derived

//   object WriteToMapOps {
//     extension [T](value: T)(using wtm: WriteToMap[T]) // <: Product
//       def writeToMap: Map[String, Any] =
//         wtm.writeToMap(value) match
//           case Node(map) => map
//   }

//   trait WriteToMap[T]:
//     def writeToMap(value: T): WriteOutput

//   object WriteToMap {

//     inline def summonWriter[T]: WriteToMap[T] =
//       summonInline[WriteToMap[T]]

//     given writeInt: WriteToMap[Int] with
//       def writeToMap(value: Int) = WriteOutput.Leaf(value)

//     given writeString: WriteToMap[String] with
//       def writeToMap(value: String) = WriteOutput.Leaf(value)

//     given writeList[T](using WriteToMap[T]): WriteToMap[List[T]] with
//       def writeToMap(value: List[T]) = WriteOutput.Leaf(value)
    
//     inline def recurse[Names <: Tuple, Types <: Tuple](element: Product)(index: Int): Map[String, WriteOutput] =
//       inline erasedValue[(Names, Types)] match
//         case (_: (name *: names), _: (tpe *: types)) =>
//           val key = constValue[name].toString
//           val value = element.productElement(index).asInstanceOf[tpe]
//           val next = recurse[names, types](element)(index + 1)
//           summonWriter[tpe].writeToMap(value) match
//             case Leaf(value) => next + (key -> value)
//             case Node(map) => next ++ map
//         case (_: EmptyTuple, _) =>
//           Map.empty[String, Any]

//     /** if you change to 'inline given' here, you will get the following error in UseDerivedImmutable:
//      * {{
//      * value toMap is not a member of Person.
//      * An extension method was tried, but could not be fully constructed:
//      * 
//      *     org.deusaquilus.DerivedImmutable.WriteToMapOps.toMap[Person](p)(
//      *       ambiguous: both method writeArbitraryToMap in object DerivedImmutable and method derived in object WriteToMap match type org.deusaquilus.DerivedImmutable.WriteToMap[Person]
//      *         summon[org.deusaquilus.DerivedImmutable.WriteToMap[Person]]
//      *     )
//      * }}
//      */

//     inline def derived[T]: WriteToMap[T] =
//       inline summonInline[Mirror.ProductOf[T]] match
//         case proMir: Mirror.ProductOf[T] =>
//           new WriteToMap[T]:
//             def writeToMap(value: T): WriteOutput =
//               Node(recurse[proMir.MirroredElemLabels, proMir.MirroredElemTypes](value.asInstanceOf[Product])(0))
//         case _ =>
//           throw new IllegalArgumentException(s"No mirror found for ${summonInline[Type[T]]}")
    
//   }
// }