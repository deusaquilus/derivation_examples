package org.deusaquilus

import scala.quoted._

object PrintMac {
  inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }
  def printMacImpl(any: Expr[Any])(using qctx: Quotes): Expr[Unit] = {
    import qctx.reflect._
    println(Printer.TreeShortCode.show(any.asTerm.underlyingArgument))
    '{ () }
  }
}