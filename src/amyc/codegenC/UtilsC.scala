package amyc
package codegenC

import ast.Identifier
import c.{Function, Parameter}
import c.Instructions._
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

  val memoryBoundary = 0

  // # of global variables
  val globalsNo = 1

  // The default includes we will pass to a c Module
  val defaultIncludes: List[String] = List(
    "stdio",
    "stdlib",
    "string"
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
    Const(4* (index + 1)) <:> Add
  }

  // Increment a local variable
  def incr(local: Int): Code = {
    GetLocal(local) <:> Const(1) <:> Add <:> SetLocal(local)
  }

  // A fresh label name
  def getFreshLabel(name: String = "label") = {
    Identifier.fresh(name).fullName
  }

  // Creates a known string constant s in memory
  def mkString(s: String): Code = ???

  // Built-in implementation of concatenation
  val concatImpl: Function = {
    val param1 = new Parameter("s1", CStringType, true)
    val param2 = new Parameter("s2", CStringType, true)
    Function("concat", List(param1, param2), CStringType)({
      Const(12)
    })
  }

  //val digitToStringImpl: Function = ???

  //val readStringImpl: Function = ???

  val cFunctions = List(concatImpl)/*List(concatImpl, digitToStringImpl, readStringImpl)*/

  implicit def toCArgs(args: List[ParamDef]) = args.map(a => new Parameter(a.name, a.tt.tpe))
  implicit def i2s(i: Name): String = i.name
  implicit def toCType(tpe: Type): CType = tpe match {
    case IntType => CIntType
    case _ => CStringType
  }

}
