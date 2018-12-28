package amyc.c

import scala.language.implicitConversions
import amyc.c.Instructions._
import amyc.codegenC.UtilsC.CType
import amyc.utils._

// Printer for C modules
object ModulePrinter {
  private implicit def s2d(s: String) = Raw(s)

  private def mkMod(mod: Module): Document = Stacked(
    Stacked(mod.imports map mkImport),
    Stacked(mod.functions map mkFun)
  )

  private def mkImport(s: String): Document =
    Lined(List("#include <", s, ".h>"))

  private def mkFun(fh: Function): Document = {
    val retType = fh.retType.toString
    val paramsDoc: Document = if (fh.args == 0) "" else {
      Lined(List(
        "(",
        Lined(fh.args.map(mkParam), ", "),
        ") "
      ))
    }

    Stacked(
      "",
      Lined(List(s"${retType} ${fh.name} ", paramsDoc)),
      "{",
      Indented(Stacked(mkCode(fh.code))),
      "}"
    )
  }

  private def mkParam(param: Parameter): Document = {
    val tpe = param.tpe.toString
    val const = if (param.const) "const " else ""
    Raw(const ++ tpe ++ " " ++ param.name)
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
      case Call(_, params, semcol) =>
        val parameters = params.map(mkCode).map(d => Lined(d))
        val semCol: List[Document] = if (semcol) List(mkInstr(SemCol)) else List("")
        Lined(mkInstr(h) :: Raw("(") :: Lined(parameters, ", ") :: List(Raw(")")) ::: semCol) ::
        mkCode(t)
      case SetLocal(_, _, value, _) =>
        Lined(mkInstr(h) :: mkCode(value) ::: List(mkInstr(SemCol))) ::
        mkCode(t)
      case AllocateMem(size) =>
        Lined(mkInstr(h) :: Raw("(") :: mkCode(size)  ::: List(Raw(")"))) ::
        mkCode(t)
      case infix: Infix =>
        Lined(mkCode(infix.lhs) ::: List(mkInstr(h)) ::: mkCode(infix.rhs)) ::
          mkCode(t)
      case _ =>
        mkInstr(h) ::
        mkCode(t)
    }
  }

  private def mkInstr(instr: Instruction): Document = instr match {
    case Const(value) => s"$value"
    case Add(_, _) => " + "
    case Sub(_, _) => " - "
    case Mul(_, _) => " * "
    case Div(_, _) => " / "
    case Rem(_, _) => " % "
    case And(_, _) => " && "
    case Or(_, _)  => " || "
    case Eqz => "i32.eqz"
    case Lt_s(_, _) => " < "
    case Le_s(_, _) => " <= "
    case Eq(_, _) => " == "
    case Drop => "drop"
    case If_void => "if ()"
    case If_i32 => "if (result i32)"
    case Else => "else {"
    case Block(label) => s"block $$$label"
    case Loop(label) => s"loop $$$label"
    case Br(label)=> s"br $$$label"
    case Return => "ret"
    case End => "end"
    case Call(name, _, _) => name
    case Unreachable => "unreachable"
    case GetLocal(name) => name
    case SetLocal(name, tpe, _, constant) =>
      val const = if (constant) "const " else ""
      s"$const$tpe $name = "
    case GetGlobal(index) => s"get_global $index"
    case SetGlobal(index) => s"set_global $index"
    case Store => "i32.store"
    case Load => "i32.load"
    case Store8 => "i32.store8"
    case Load8_u => "i32.load8_u"
    case Strng(s) => s
    case SemCol => ";"
    case AllocateMem(_) => "malloc"
  }

  def apply(mod: Module) = mkMod(mod).print
  def apply(fh: Function) = mkFun(fh).print
  def apply(instr: Instruction) = mkInstr(instr).print

}
