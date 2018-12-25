package amyc
package c

import Instructions.Code
import codegenC.Utils._

class Function private (val name: String, val args: Int, val retType: CType, val locals: Int, val code: Code) {
  override def toString: String = ModulePrinter(this)
}

class LocalsHandler(args: Int) {
  private var locals_ = 0
  def getFreshLocal(): Int = {
    locals_ += 1
    args + locals_ - 1
  }
  private[c] def locals = locals_
}

object Function {
  def apply(name: String, args: Int, retType: CType = CIntType)(codeGen: LocalsHandler => Code) = {
    val lh = new LocalsHandler(args)
    // Make code first, as it may increment the locals in lh
    val code = codeGen(lh)
    new Function(name, args, retType, lh.locals, code)
  }
}
