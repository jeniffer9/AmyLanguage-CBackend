package amyc.c

import scala.language.implicitConversions

class CaseClass private (val name: String, val owner: String, val fields: List[Parameter], val index: Int, val tpe: String) {
  override def toString: String = ModulePrinter(this)
}

object CaseClass {
  def apply(className: String, owner: String, fields: List[Parameter], index: Int, tpe: String) = {
    new CaseClass(className, owner, fields, index, tpe)
  }
}