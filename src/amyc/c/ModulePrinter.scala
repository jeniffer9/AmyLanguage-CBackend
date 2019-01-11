package amyc.c

import scala.language.implicitConversions
import amyc.c.Instructions._
import amyc.codegenC.UtilsC._
import amyc.utils._

// Printer for C modules
object ModulePrinter {
  private implicit def s2d(s: String) = Raw(s)
  private def abstractName(s: String) = "Abstract_" + s

  private def mkMod(mod: Module): Document = Stacked(
    Stacked(mod.imports map mkImport),
    Stacked(""),
    Stacked(mod.types map mkAbstractClass),
    Stacked(""),
    Stacked(mod.classes map mkCaseClass, true),
    Stacked(""),
    Stacked(mod.functions.dropRight(1) map mkDeclaration),
    Stacked(mod.functions map mkFun)
  )

  private def mkImport(s: String): Document =
    Lined(List("#include <", s, ".h>"))

  private def mkDeclaration(fh: Function): Document = {
    val retType = fh.retType.toString
    val paramsDoc: Document = if (fh.args == 0) "" else {
      Lined(List(
        "(",
        Lined(fh.args.map(mkParam), ", "),
        ")"
      ))
    }
    Stacked(
      Lined(List(s"${retType} ${fh.name}", paramsDoc, mkInstr(SemCol))),
    )
  }

  private def mkAbstractClass(ac: AbstractClass): Document = {
    val enumName = ac.name.toUpperCase ++ "_CASE"
    Stacked(
      Lined(List("typedef enum {",
        Lined(ac.CaseClasses.map(i => Raw(i.name.toUpperCase)), ", ")
        , "} ", enumName, ";")
      ),
      "",
      Lined(List("typedef struct ", ac.name, " {")),
      Indented(Stacked(
        "void* instance;",
        Lined(List(Raw(enumName), " caseClass;")),
      )),
      Lined(List("} ", Raw(abstractName(ac.name) + ", "), Raw("*" + ac.name), ";"))
    )
  }

  private def mkCaseClass(cc: CaseClass): Document = {
    val paramNames: List[String] = cc.fields.map(_.name)
    val valName: String = cc.name.split('_')(1).toLowerCase
    val tpe: String = cc.owner + "_" + cc.tpe
    Stacked(
      Lined(List("typedef struct ", cc.name, " {")),
      Indented(Stacked(
        //TODO: check when no parameters e.g. Nul case
        cc.fields.map(f => Lined(List(mkParam(f), Raw(";"))))
      )),
      Lined(List("} ", Raw(cc.name), ";")),
      "",
      mkFun(Function(cc.name + "_Constructor", cc.fields, StructType(tpe)){
        SetLocal(valName, Pointer(StructType(cc.name)), AllocateMem(GetLocal("sizeof("+cc.name+")"))) <:>
        cc.fields.map(f => SetProperty(valName, f.name, f.name)) <:> GetLocal("") <:>
        SetLocal(cc.tpe.toLowerCase, StructType(tpe), AllocateMem(GetLocal("sizeof("+abstractName(tpe)+")"))) <:>
        SetProperty(cc.tpe.toLowerCase, "instance", valName) <:>
          SetProperty(cc.tpe.toLowerCase, "caseClass", cc.index.toString) <:>
          GetLocal("") <:> Return(GetLocal(cc.tpe.toLowerCase))
      }),
      "",
      Raw("#define instance_"+valName+"(abstr_class)(("+Pointer(StructType(tpe))+")abstr_class->instance")
    )
  }

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
      Lined(List(s"${retType} ${fh.name}", paramsDoc)),
      "{",
      Indented(Stacked(mkCode(fh.code))),
      "}"
    )
  }

  private def mkParam(param: Parameter): Document = {
    val tpeS = param.tpe.toString
    val tpe = if(param.module != null && (param.tpe match {case StructType(_) => true case _ => false})) //fullName for structs params
      param.module ++ "_" ++ tpeS
      else tpeS
    val const = if (param.const) "const " else ""
    Raw(const ++ tpe ++ " " ++ param.name)
  }

  private def mkCode(code: Code): List[Document] = code.instructions match {
    case Nil => Nil
    case h :: t => h match {
      case Unit => mkCode(t)
      case OneLiner(c) =>
        Lined(mkCode(c) ::: List(mkInstr(SemCol))) ::
        mkCode(t)
      case If(_) =>
        mkInstr(h) ::
          (mkCode(t) map Indented)
      case Else =>
        Unindented(mkInstr(h)) ::
        mkCode(t)
      case End =>
        Unindented(mkInstr(h)) ::
        (mkCode(t) map Unindented)
      case Call(_, params, semcol) =>
        val parameters = params.map(mkCode).map(d => Lined(d))
        val semCol: List[Document] = if (semcol) List(mkInstr(SemCol)) else List("")
        Lined(mkInstr(h) :: Raw("(") :: Lined(parameters, ", ") :: List(Raw(")")) ::: semCol) ::
        mkCode(t)
      case Constructor(args) =>
        val argos = args.map(mkCode).map(d => Lined(d))
        Lined(Raw("{") :: Lined(argos, ", ") :: List(Raw("}"))) ::
        mkCode(t)
      case SetLocal(_, _, value, _) =>
        Lined(mkInstr(h) :: mkCode(value) ::: List(mkInstr(SemCol))) ::
        mkCode(t)
      case AllocateMem(size) =>
        Lined(mkInstr(h) :: Raw("(") :: mkCode(size)  ::: List(Raw(")"))) ::
        mkCode(t)
      case prefix: Prefix =>
        Lined(mkInstr(h) :: Raw("(") :: mkCode(prefix.body) ::: List(Raw(")"))) ::
        mkCode(t)
      case infix: Infix =>
        Lined(mkCode(infix.lhs) ::: List(mkInstr(h)) ::: mkCode(infix.rhs)) ::
          mkCode(t)
      case Return(code) =>
        Lined(mkInstr(h) :: mkCode(code) ::: List(mkInstr(SemCol))) ::
        mkCode(t)
      case Seq(c1) =>
        Lined(mkCode(c1) ::: List(mkInstr(SemCol))) ::
        mkCode(t)
      case Switch(scrut, cases) =>
        Lined("switch(" :: Lined(mkCode(scrut)) :: List(Raw("){"))) ::
          cases.map(c => Stacked(mkCode(c))) ::: mkCode(t)
      case _ =>
        mkInstr(h) ::
        mkCode(t)
    }
  }

  private def mkInstr(instr: Instruction): Document = instr match {
    case SetProperty(of, prop, to) => s"$of->$prop = $to;"
    case Case(c) => Lined("case " :: mkCode(c))
    case Const(value) => s"$value"
    case Add(_, _) => " + "
    case Sub(_, _) => " - "
    case Mul(_, _) => " * "
    case Div(_, _) => " / "
    case Rem(_, _) => " % "
    case And(_, _) => " && "
    case Or(_, _)  => " || "
    case Lt_s(_, _) => " < "
    case Le_s(_, _) => " <= "
    case Eq(_, _) => " == "
    case Not(_) => "!"
    case Neg(_) => "-"
    case If(cond) => Lined(Raw("if (") :: mkCode(cond) ::: List(Raw(") {")))
    case Else => "} else {"
    case Return(_) => "return "
    case End => "}"
    case Call(name, _, _) => name
    case GetLocal(name) => name
    case SetLocal(name, tpe, _, constant) =>
      val const = if (constant) "const " else ""
      s"$const$tpe $name = "
    case Strng(s) => "\"" + s + "\""
    case SemCol => ";"
    case AllocateMem(_) => "malloc"
    case True => "true"
    case False => "false"
  }

  def apply(mod: Module) = mkMod(mod).print
  def apply(fh: Function) = mkFun(fh).print
  def apply(ac: AbstractClass) = mkAbstractClass(ac).print
  def apply(cc: CaseClass) = mkCaseClass(cc).print
  def apply(instr: Instruction) = mkInstr(instr).print

}
