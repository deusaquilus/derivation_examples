package org.deusaquilus.ex3_mirrorsummon

import scala.collection.mutable

object UseDerivedMirrorSummon {
  import DerivedMirrorSummon._
  import WriteToMapOps._

  def main(args: Array[String]): Unit = {
    caseClassWithList()
  }

  def caseClassWithList() = {
    // Works
    println(CaseClassWithList.derivedWithListLeaf.toMap)

    // Does not work
    //println(CaseClassWithList.derivedWithListNode().toMap)
  }

  object CaseClassWithList {
    given writeList[T](using wtm: WriteToMap[T]): WriteToMap[List[T]] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, values: List[T]): Unit =
        val valueKeys = 
          values.map(v => {
            val newMap = mutable.Map[String, Any]()
            wtm.writeToMap(newMap)("k", v)
            println(s"Wrote to a map: ${newMap.toMap}")
            newMap("k")
          })
        map.put(key, valueKeys)

    // Works
    def derivedWithListLeaf = {
      case class Person(firstName: String, lastName: String, nicknames: List[String])
      val p = Person("Yosef", "Bloggs", List("Joseph", "Joe"))  
      p.writeToMap
    }

    // Does not work
    def derivedWithListNode = {
      case class Address(street: String, zip: Int)
      case class Person(firstName: String, lastName: String, nicknames: List[Address])
      val p = 
        Person("Yosef", "Bloggs", List(
          Address("123 Place", 11122), 
          Address("456 Ave", 11122))
        )
      p.writeToMap
    }
  }
}