package amyc.c

import amyc.codegenC.UtilsC.CType

import scala.language.implicitConversions

// A subset of instructions defined by the WASM standard
object Instructions {
  abstract class Instruction

  case class Const(value: Int) extends Instruction
  case class Strng(s: String) extends Instruction

  trait Infix extends Instruction{
    val lhs: Code
    val rhs: Code
  }

  trait Prefix extends Instruction {
    val body: Code
  }

  // Numeric/logical instructions (all take i32 operands)
  case class Add(lhs: Code, rhs: Code) extends Infix
  case class Sub(lhs: Code, rhs: Code) extends Infix
  case class Mul(lhs: Code, rhs: Code) extends Infix
  case class Div(lhs: Code, rhs: Code) extends Infix
  case class Rem(lhs: Code, rhs: Code) extends Infix
  case class And(lhs: Code, rhs: Code) extends Infix
  case class Or(lhs: Code, rhs: Code) extends Infix
  case object Eqz  extends Instruction // Return 1 if operand is 0, 0 otherwise
  case class Lt_s(lhs: Code, rhs: Code) extends Infix // Signed less-than
  case class Le_s(lhs: Code, rhs: Code) extends Infix // Signed less-equals
  case class Eq(lhs: Code, rhs: Code) extends Infix

  case class Not(body: Code) extends Prefix
  case class Neg(body: Code) extends Prefix

  case object SemCol extends Instruction
  case object True extends Instruction
  case object False extends Instruction

  // Control instructions
  case class If(cond: Code) extends Instruction
  case object Else extends Instruction // Marks the end of the implicit 'then' of an if-block
  case object End extends Instruction // Marks the end of an if-then-else or block
  case class Call(name: String, params: List[Code], semcol: Boolean = false) extends Instruction
  case class Return(code: Code) extends Instruction
  case class Seq(e1: Code, e2: Code) extends Instruction

  // Locals (parameters, local variables)
  case class GetLocal(name: String) extends Instruction
  case class SetLocal(name: String, tpe: CType, value: Code, const: Boolean = false) extends Instruction
  case class AllocateMem(size: Code) extends Instruction

  // Represents a sequence of instructions
  case class Code(instructions: List[Instruction]) {
    def <:>(i: Instruction) = Code(instructions :+ i)
    def <:>(other: Code) = Code(this.instructions ++ other.instructions)
  }

  // Useful implicit conversions to construct Code objects
  implicit def i2c(i: Instruction): Code = Code(List(i))
  implicit def is2c(is: List[Instruction]): Code = Code(is)
  implicit def cs2c(cs: List[Code]): Code = Code(cs flatMap (_.instructions))
}
