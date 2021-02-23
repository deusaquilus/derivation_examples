package org.deusaquilus

object UseMacroBased {
  import MacroBased._

  def main(args: Array[String]): Unit = {

  }

  def macroBased(): Unit = {
    RunQuery.apply[Customer]("uri/to/db") //hello
  }
}