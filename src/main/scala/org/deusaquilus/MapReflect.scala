package org.deusaquilus

import scala.collection.mutable;
import scala.quoted._
import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
import scala.deriving._

object Util {
  case class MediumObject(field1: Int, field2: Int, field3: Int, field4: Int, field5: Int, field6: Int, field7: Int, field8: Int, field9: Int, field10: Int)
  object MediumObject {
    def random: MediumObject = MediumObject(rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand())

  }
  def rand() = scala.util.Random.nextInt()

  case class Person(firstName: String, lastName: String, age: Int)

  def reflectCaseClassFields[T <: Product](p: T) = {
    val fieldNames = p.getClass.getDeclaredFields.map(_.getName).toSet
    p.getClass.getDeclaredMethods.filter(m => fieldNames.contains(m.getName)).toList
  }
}

object DerivedNaive {
  trait WriteToMap[T]:
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit

  given writeInt: WriteToMap[Int] with
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: Int): Unit = map.put(key, value)

  given writeString: WriteToMap[String] with
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: String): Unit = map.put(key, value)

  inline def recurseNames[Names <: Tuple]: List[String] =
    inline erasedValue[Names] match
      case _: (name *: names) =>
        constValue[name].toString +: recurseNames[names]

  inline def summonWriter[T]: WriteToMap[T] =
    summonInline[WriteToMap[T]]
  
  inline def recurseTypes[Types <: Tuple]: List[WriteToMap[Any]] =
    inline erasedValue[Types] match
      case _: (tpe *: types) =>
        summonWriter[tpe].asInstanceOf[WriteToMap[Any]] +: recurseTypes[types]

  inline def derived[T](using mir: Mirror.Of[T]) = new WriteToMap[T] {
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit =
      inline mir match
        case proMir: Mirror.ProductOf[T] =>
          val valueProd = value.asInstanceOf[Product]           // it has a Product Mirror, must be a Product!
          val names = recurseNames[proMir.MirroredElemLabels]   // List(firstName, lastName, age)
          val writers = recurseTypes[proMir.MirroredElemTypes]  // List(writeString, writeString, writeInt)
          names.zip(writers).zipWithIndex.map {
            case ((name, writer), index) =>
              writer.writeToMap(map)(name, valueProd.productElement(index))
          }
        case _ =>
          throw new IllegalArgumentException(s"No mirror found for ${value}")
  }
}

object Derived {
  trait WriteToMap[T]:
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit

  inline def summonWriter[T]: WriteToMap[T] =
    summonInline[WriteToMap[T]]
  
  inline def recurse[Names <: Tuple, Types <: Tuple](element: Product, map: mutable.Map[String, Any])(index: Int): Unit =
    inline erasedValue[(Names, Types)] match
      case (_: (name *: names), _: (tpe *: types)) =>
        val key = constValue[name].toString
        val value = element.productElement(index).asInstanceOf[tpe]
        summonWriter[tpe].writeToMap(map)(key, value)
        recurse[names, types](element, map)(index + 1)
      case (_: EmptyTuple, _) =>
        // Ignore

  inline def base[T](using mir: Mirror.Of[T]) = new WriteToMap[T] {
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit =
      inline mir match
        case proMir: Mirror.ProductOf[T] =>
          recurse[proMir.MirroredElemLabels, proMir.MirroredElemTypes](value.asInstanceOf[Product], map)(0)
        case _ =>
          throw new IllegalArgumentException(s"No mirror found for ${value}")
  }
}

object Main {
  import Util._

  def main(args: Array[String]): Unit = {
    println("=============Using Reflection=============")
    funA()
    println("=============Using Manual=============")
    funB()
    println("=============Using Macros=============")
    funMac()
  }

  def funDerive() = {

  }

  def funMac() = {
    val p = Person("Joe", "Bloggs", 123)

    val numTests = 100000
    val numTestsSkip = 0
    var totalTime: Long = 0
    
    // just a variable to do something with that value of the map so compiler won't get rid of it
    var dummy: Long = 0

    for (i <- (1 to numTests + numTestsSkip)) {
      val map = mutable.Map[String, Any]() //.sizeHint(fields, 0)
      var start: Long = System.currentTimeMillis();

      MacUtil.populateMap(p, map)

      if (i > numTestsSkip) {
        totalTime = totalTime + (System.currentTimeMillis - start);
        if (i % 10000 == 0) {
          println(s"Average Time: ${(totalTime * 1000000).toDouble/(i - numTestsSkip).toDouble} - test: ${i} - total time: ${totalTime}")
          dummy += map.size
        }
      }
    }
  }

  def funB() = {
    val p = Person("Joe", "Bloggs", 123)

    val numTests = 100000
    val numTestsSkip = 0
    var totalTime: Long = 0
    
    // just a variable to do something with that value of the map so compiler won't get rid of it
    var dummy: Long = 0

    for (i <- (1 to numTests + numTestsSkip)) {
      val map = mutable.Map[String, Any]() //.sizeHint(fields, 0)
      var start: Long = System.currentTimeMillis();

      map.put("firstName",  p.firstName)
      map.put("lastName",  p.lastName)
      map.put("age",  p.age)

      if (i > numTestsSkip) {
        totalTime = totalTime + (System.currentTimeMillis - start);
        if (i % 10000 == 0) {
          println(s"Average Time: ${(totalTime * 1000000).toDouble/(i - numTestsSkip).toDouble} - test: ${i} - total time: ${totalTime}")
          dummy += map.size
        }
      }
    }
  }

  def funA() = {
    val p = Person("Joe", "Bloggs", 123)
    val fields = reflectCaseClassFields(p)

    val numTests = 100000
    val numTestsSkip = 0
    var totalTime: Long = 0
    
    // just a variable to do something with that value of the map so compiler won't get rid of it
    var dummy: Long = 0

    for (i <- (1 to numTests + numTestsSkip)) {
      val map = mutable.Map[String, Any]() //.sizeHint(fields, 0)
      var start: Long = System.currentTimeMillis();

      for (field <- fields) {
        map.put(field.getName,  field.invoke(p))
      }
      if (i > numTestsSkip) {
        totalTime = totalTime + (System.currentTimeMillis() - start);
        if (i % 10000 == 0) {
          println(s"Average Time: ${(totalTime * 1000000).toDouble/(i - numTestsSkip).toDouble} - test: ${i} - total time: ${totalTime}")
          dummy += map.size
        }
      }
    }
  }
}

