package amyc.c

import scala.language.implicitConversions

class AbstractClass private (val name: String) {
  override def toString: String = ModulePrinter(this)
}

object AbstractClass {
  def apply(typeName: String) = {
    new AbstractClass(typeName)
  }
}


