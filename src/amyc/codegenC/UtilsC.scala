package amyc
package codegenC

import ast.Identifier
import c.{Function, Parameter}
import c.Instructions.{Call, _}
import ast.SymbolicTreeModule._

import scala.language.implicitConversions
// Utilities for CodeGen
object UtilsC {

  trait CType
  case object CStringType extends CType {
    override def toString: String = "char*"
  }
  case object CIntType extends CType {
    override def toString: String = "int"
  }
  case object CBoolType extends CType {
    override def toString: String = "bool"
  }
  case object CSizeT extends CType {
    override def toString: String = "size_t"
  }
  case object CVoid extends CType {
    override def toString: String = "void"
  }
  case class StructType(val name: String) extends CType {
    override def toString: String = name
  }

  val memoryBoundary = 0

  // # of global variables
  val globalsNo = 1

  // The default includes we will pass to a c Module
  val defaultIncludes: List[String] = List(
    "stdio",
    "stdlib",
    "string",
    "stdbool"
  )

  // We don't generate code for these functions in CodeGen (they are hard-coded here or in js wrapper)
  val builtInFunctions: Set[String] = Set(
    "Std_printInt",
    "Std_printString",
    "Std_digitToString",
    "Std_readInt",
    "Std_readString"
  )

  /** Utilities */
  // A globally unique name for definitions
  def fullName(owner: Identifier, df: Identifier): String = owner.name + "_" + df.name

  // Given a pointer to an ADT on the top of the stack,
  // will point at its field in index (and consume the ADT).
  // 'index' MUST be 0-based.
  def adtField(index: Int): Code = {
    Const(4* (index + 1))
  }

  // A fresh label name
  def getFreshLabel(name: String = "label") = {
    Identifier.fresh(name).fullName
  }

  // Creates a known string constant s in memory
  def mkString(s: String): Code = ???

  // Built-in implementation of concatenation
  val concatImpl: Function = {
    val local4 = "s1"
    val local5 = "s2"
    val param1 = new Parameter(local4, CStringType, true)
    val param2 = new Parameter(local5, CStringType, true)
    Function("String_concat", List(param1, param2), CStringType)({

      val strLenCall1 = Call("strlen", List(GetLocal(local4)))
      val strLenCall2 = Call("strlen", List(GetLocal(local5)))

      val local1 = "len1"
      val local2 = "len2"
      val local3 = "result"
      val allocateMem = AllocateMem(Add(GetLocal(local1), Add(GetLocal(local2), Const(1))))

      val memCpyCall1 = Call("memcpy", List(GetLocal(local3), GetLocal(local4), GetLocal(local1)), true)
      val memCpyCall2 = Call("memcpy", List(Add(GetLocal(local3), GetLocal(local1)), GetLocal(local5), Add(GetLocal(local2), Const(1))), true)

      SetLocal(local1, CSizeT, strLenCall1, true) <:>
        SetLocal(local2, CSizeT, strLenCall2, true) <:>
        SetLocal(local3, CStringType, allocateMem) <:>
        memCpyCall1 <:> memCpyCall2 <:>
        Return(GetLocal(local3))

    })
  }

  val printStringImpl: Function = {
    val paramString = "%s\\n"
    val local1 = "string"
    val param = new Parameter(local1, CStringType)

    Function("Std_printString", List(param), CVoid) {
      Call("printf", List(paramString, GetLocal(local1)), true)
    }
  }

  val printIntImpl: Function = {
    val paramInt = "%d\\n"
    val local1 = "integer"
    val param = new Parameter(local1, CIntType)

    Function("Std_printInt", List(param), CVoid) {
      Call("printf", List(paramInt, GetLocal(local1)), true)
    }
  }

  val digitToStringImpl: Function = {
    val paramDigit = "%d"
    val local1 = "digit"
    val param = new Parameter(local1, CIntType)

    val local2 = "string"

    Function("Std_digitToString", List(param), CStringType) {
      SetLocal(local2, CStringType, AllocateMem(Call("sizeof", List(GetLocal("int"))))) <:>
      Call("sprintf", List(GetLocal(local2), paramDigit, GetLocal(local1)), true) <:>
      Return(GetLocal(local2))
    }
  }

  val readStringImpl: Function = {
    val paramString = "%s"
    val local1 = "string"
    val param = new Parameter(local1, CStringType)

    Function("Std_readString", List(param), CVoid) {
      Call("printf", List(paramString, GetLocal(local1)), true)
    }
  }

  val cFunctions = List(concatImpl, printStringImpl, printIntImpl, digitToStringImpl)/*List(concatImpl, digitToStringImpl, readStringImpl)*/

  implicit def toCArgs(args: List[ParamDef]): List[Parameter] = args.map(a => new Parameter(a.name, a.tt.tpe))
  implicit def i2s(i: Name): String = i.name
  implicit def toCType(tpe: Type): CType = tpe match {
    case IntType => CIntType
    case BooleanType => CBoolType
    case UnitType => CVoid
    case ClassType(qname) => StructType(qname)
    case _ => CStringType
  }
  implicit def s2is(s: String): Code = Strng(s)

}
