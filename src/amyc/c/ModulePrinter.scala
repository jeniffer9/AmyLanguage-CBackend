package amyc.c

import scala.language.implicitConversions
import amyc.c.Instructions._
import amyc.utils._

// Printer for C modules
object ModulePrinter {
  private implicit def s2d(s: String) = Raw(s)

  private def mkMod(mod: Module): Document = Stacked(
    Indented(Stacked(mod.imports map mkImport)),
    //Indented("(global (mut i32) i32.const 0) " * mod.globals),
    Indented(Stacked(mod.functions map mkFun))
  )

  private def mkImport(s: String): Document =
    Lined(List("#include <", s, ".h>"))

  private def mkFun(fh: Function): Document = {
    val name = fh.name
    val retType = fh.retType.toString
    val paramsDoc: Document = if (fh.args == 0) "" else {
      Lined(List(
        "(",
        Lined(List.fill(fh.args)(Raw("I'm a para")), " "),
        ") "
      ))
    }

    Stacked(
      Lined(List(s"${retType} ${fh.name} ", paramsDoc)),
      "{",
      Indented(Stacked(mkCode(fh.code))),
      "}"
    )
  }

  private def mkCode(code: Code): List[Document] = code.instructions match {
    case Nil => Nil
    case h :: t => h match {
      case Else =>
        Unindented(mkInstr(h)) ::
        mkCode(t)
      case End =>
        Unindented(mkInstr(h)) ::
        (mkCode(t) map Unindented)
      case If_void | If_i32 | Block(_) | Loop(_) =>
        mkInstr(h) ::
        (mkCode(t) map Indented)
      case _ =>
        mkInstr(h) ::
        mkCode(t)
    }
  }

  private def mkInstr(instr: Instruction): Document = {
    instr match {
      case Const(value) => s"$value"
      case Add => " + "
      case Sub => " - "
      case Mul => " * "
      case Div => " / "
      case Rem => " % "
      case And => " && "
      case Or  => " || "
      case Eqz => "i32.eqz"
      case Lt_s => " < "
      case Le_s => " <= "
      case Eq => " == "
      case Drop => "drop"
      case If_void => "if"
      case If_i32 => "if (result i32)"
      case Else => "else"
      case Block(label) => s"block $$$label"
      case Loop(label) => s"loop $$$label"
      case Br(label)=> s"br $$$label"
      case Return => "ret"
      case End => "end"
      case Call(name) => s"call $$$name"
      case Unreachable => "unreachable"
      case GetLocal(index) => s"get_local $index"
      case SetLocal(index) => s"set_local $index"
      case GetGlobal(index) => s"get_global $index"
      case SetGlobal(index) => s"set_global $index"
      case Store => "i32.store"
      case Load => "i32.load"
      case Store8 => "i32.store8"
      case Load8_u => "i32.load8_u"
    }
  }

  def apply(mod: Module) = mkMod(mod).print
  def apply(fh: Function) = mkFun(fh).print
  def apply(instr: Instruction) = mkInstr(instr).print

}
