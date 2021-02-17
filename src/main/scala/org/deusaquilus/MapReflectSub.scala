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

  // toto define for int, string, long etc....
}

object MainSub {
  import Util._

  def reflectCaseClassFields[T <: Product](p: T) = {
    val fieldNames = p.getClass.getDeclaredFields.map(_.getName).toSet
    p.getClass.getDeclaredMethods.filter(m => fieldNames.contains(m.getName)).toList
  }

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

