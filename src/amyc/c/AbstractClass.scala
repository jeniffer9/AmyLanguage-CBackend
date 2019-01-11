package amyc.c

import amyc.ast.Identifier

import scala.language.implicitConversions

class AbstractClass private (val name: String, val CaseClasses: List[Identifier]) {
  override def toString: String = ModulePrinter(this)
}

object AbstractClass {
  def apply(typeName: String, args: List[Identifier]) = {
    new AbstractClass(typeName, args)
  }
}


