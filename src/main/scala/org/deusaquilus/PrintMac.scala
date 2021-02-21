package org.deusaquilus

import scala.quoted._

// object PrintMac {
//   inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }
//   def printMacImpl(any: Expr[Any])(using qctx: Quotes): Expr[Unit] = {
//     import qctx.reflect._
//     println(Printer.TreeShortCode.show(any.asTerm.underlyingArgument))
//     '{ () }
//   }
// }

// object PrintMacPass {
//   inline def apply[T](inline any: T): T = ${ printMacImpl('any) }
//   def printMacImpl[T: Type](any: Expr[T])(using qctx: Quotes): Expr[T] = {
//     import qctx.reflect._
//     println(Printer.TreeShortCode.show(any.asTerm))
//     any
//   }
// }