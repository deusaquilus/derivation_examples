package org.deusaquilus.prereq

import scala.deriving._
import scala.quoted._
import scala.compiletime.{ erasedValue, codeOf, summonInline }

object TypeMatching {
  def main(args: Array[String]): Unit = {
    println(matchTup[(String, Int)])
    println(matchSummonRecurse[(String, Int, String)])
  }

  inline def matchType[T]: String =
    inline erasedValue[String] match
      case _: String => "It's a String"
      case _: Int => "It's an Int"

  inline def matchTup[T] =
    inline erasedValue[T] match
      case _: (String *: suffix) =>
        "String member with suffix"

  type IsProduct[T <: Product]
  inline def matchTupType[T] =
    inline erasedValue[T] match
      case _: (IsProduct[prefix] *: suffix) =>
        "Prefix is a product"


  trait Fooable[T]:
    def fooIt: String
  given Fooable[String] with
    def fooIt = "String-Fooable"
  given Fooable[Int] with
    def fooIt = "Int-Fooable"

  inline def matchSummonType[T] =
    inline erasedValue[T] match
      case _: (prefix *: suffix) =>
        summonInline[Fooable[prefix]].fooIt


  inline def matchSummonRecurse[T]: List[String] =
    inline erasedValue[T] match
      case _: (prefix *: suffix) =>
        summonInline[Fooable[prefix]].fooIt +: matchSummonRecurse[suffix]
      case _: EmptyTuple =>
        Nil
  
}