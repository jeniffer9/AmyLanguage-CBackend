package amyc.c

import amyc.codegenC.UtilsC.CType

import scala.language.implicitConversions

// A subset of instructions defined by the WASM standard
object Instructions {
  abstract class Instruction

  // Load an int32 constant to the stack
  case class Const(value: Int) extends Instruction
  case class Strng(s: String) extends Instruction

  trait Infix extends Instruction {
    val lhs: Code
    val rhs: Code
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
  case object Drop extends Instruction // Drops the top value of the stack
  case object SemCol extends Instruction

  // Control instructions
  case object If_void extends Instruction // Marks the beginning of an if-block (with implicit 'then').
  case object If_i32  extends Instruction // Marks the beginning of an if-block (with implicit 'then'). Must leave an i32 on the stack
  case object Else    extends Instruction // Marks the end of the implicit 'then' of an if-block
  case object End     extends Instruction // Marks the end of an if-then-else or block
  case class Loop(label: String)  extends Instruction // A block of instructions with a label at the beginning
  case class Block(label: String) extends Instruction // A block of instructions with a label at the end
  case class Br(label: String)    extends Instruction // Jump to "label", which MUST be the label of an enclosing structure
  case class Call(name: String, params: List[Code], semcol: Boolean = false)   extends Instruction
  case object Return              extends Instruction
  case object Unreachable         extends Instruction // Always fails the program

  // Locals (parameters, local variables)
  case class GetLocal(name: String) extends Instruction
  case class SetLocal(name: String, tpe: CType, value: Code, const: Boolean = false) extends Instruction
  case class AllocateMem(size: Code) extends Instruction

  // Global variables
  case class GetGlobal(index: Int) extends Instruction
  case class SetGlobal(index: Int) extends Instruction

  // Memory
  // Stores an i32 to memory. Expects memory address, then stored value as operands
  case object Store extends Instruction
  // Loads an i32 to memory. Expects memory address as operand
  case object Load  extends Instruction
  // Stores a single byte to memory (the least significant byte of the operand)
  // Operands expected are like Store
  case object Store8 extends Instruction
  // Load a byte from memory, then zero-extend it to fill an i32
  case object Load8_u extends Instruction

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
