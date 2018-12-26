package amyc
package c

import Instructions.Code
import codegenC.UtilsC._
import ast.SymbolicTreeModule._

import scala.language.implicitConversions

class Function private (val name: String, val args: List[Parameter], val retType: CType, val code: Code) {
  override def toString: String = ModulePrinter(this)
}

class Parameter (val name: String, val tpe: CType, val const: Boolean = false)

object Function {
  def apply(name: String, args: List[Parameter], retType: CType = CIntType)(codeGen: Code) = {
    new Function(name, args, retType, codeGen)
  }
}


