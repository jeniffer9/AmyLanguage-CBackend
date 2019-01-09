package amyc.c

import scala.language.implicitConversions

class CaseClass private (val name: String, val fields: List[Parameter], val index: Int) {
  override def toString: String = ModulePrinter(this)
}

object CaseClass {
  def apply(className: String, fields: List[Parameter], index: Int) = {
    new CaseClass(className, fields, index)
  }
}





