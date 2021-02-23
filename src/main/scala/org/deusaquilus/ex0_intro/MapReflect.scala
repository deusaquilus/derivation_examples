package org.deusaquilus.ex0_intro

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
    val fieldNames = p.getClass.getDeclaredFields.map(_.getName)
    p.getClass.getDeclaredMethods.filter(m => fieldNames.contains(m.getName)).toList
  }

  val p = Person("Joe", "Bloggs", 123)
  val fieldsPre = reflectCaseClassFields(p)
}

object Main {
  import Util._

  def main(args: Array[String]): Unit = {
  }

  def useMacro() = {
    val p = Person("Joe", "Bloggs", 123)
    val map = mutable.Map[String, Any]()
    PopulateMap.populateMap(p, map)
  }

  def useFunMacro() = {
    val p = Person("Joe", "Bloggs", 123)
    val map = PopulateMapFun.populateMap(p)
  }

  def funB() = {
    val p = Person("Joe", "Bloggs", 123)

    val map = mutable.Map[String, Any]()
    map.put("firstName",  p.firstName)
    map.put("lastName",  p.lastName)
    map.put("age",  p.age)
  }

  def funA() = {
    val p = Person("Joe", "Bloggs", 123)
    val fields = reflectCaseClassFields(p)
    val map = mutable.Map[String, Any]()
    for (field <- fields)
      map.put(field.getName,  field.invoke(p))
  }
}

