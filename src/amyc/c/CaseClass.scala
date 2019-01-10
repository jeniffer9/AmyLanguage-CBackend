package amyc.c

import scala.language.implicitConversions

class CaseClass private (val name: String, val fields: List[Parameter]) {
  override def toString: String = ModulePrinter(this)
}

object CaseClass {
  def apply(className: String, fields: List[Parameter]) = {
    new CaseClass(className, fields)
  }
}





