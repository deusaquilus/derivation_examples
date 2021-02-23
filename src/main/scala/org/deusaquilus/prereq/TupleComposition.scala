package org.deusaquilus.prereq

object TupleComposition {
  def main(args: Array[String]): Unit = {

    ("foo" *: ("bar" *: (EmptyTuple))) == ("foo", "bar")
    ("foo" *: ("bar", "baz")) == ("foo", "bar", "baz")

    ("foo", "bar", "baz") match
      case (foo *: barbaz) =>
        foo == "foo"
        barbaz == ("bar", "baz")

    ("foo", 123) match
      case tup: (String *: (Int *: EmptyTuple)) => 
        println("Matched The Type")

    ("foo", 123) match
      case _: (String *: stuff) => 
        println("Tuple starts with a string")

  }
}