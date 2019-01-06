package amyc
package codegenC

import analyzer._
import ast.SymbolicTreeModule.{And => AmyAnd, Call => AmyCall, Div => AmyDiv, Or => AmyOr, Not => AmyNot, Neg => AmyNeg, _}
import ast.Identifier
import c.{Instructions, _}
import Instructions._
import UtilsC._
import utils.{Context, Pipeline}

import scala.language.implicitConversions

// Generates C code for an Amy program
object CodeGenC extends Pipeline[(Program, SymbolTable), Module] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v


    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect { case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) =>
        cgFunction(fd, name, false)
      } ++
      // Generate code for the "main" function, which contains the module expression
      optExpr.toList.map { expr =>
        val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
        cgFunction(mainFd, name, true)
      }
    }

    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the c function name from a combination of
      // module and function name, since we put everything in the same c file.
      val name = fullName(owner, fd.name)
      Function(name, fd.params, fd.retType.tpe){
        val body = cgExpr(fd.body)(ret = !(isMain || fd.retType.tpe == UnitType))
        if (isMain) {
          val (front, last) = body.instructions.splitAt(body.instructions.size-1)
          println("hello", body.instructions.head)
          println(front, "test", last)
          front <:> Seq(last) <:> Return(Const(0))
        } else {
          body
        }
      }
    }

    // Generate code for an expression expr.
    // Additional arguments are a mapping from identifiers (parameters and variables) to
    // their index in the wasm local variables, and a LocalsHandler which will generate
    // fresh local slots as required.
    def cgExpr(expr: Expr)(implicit ret: Boolean): Code = expr match {
      case retType: RetType =>
        val possibleReturnCode = retType match {
          case binaryOperator: BinaryOperator =>
            val lhs = cgExpr(binaryOperator.lhs)(false)
            val rhs = cgExpr(binaryOperator.rhs)(false)
            binaryOperator match {
              //infix instructions
              case Plus(_, _) =>
                Add(lhs, rhs)
              case Minus(_, _) =>
                Sub(lhs, rhs)
              case Times(_, _) =>
                Mul(lhs, rhs)
              case AmyDiv(_, _) =>
                Div(lhs, rhs)
              case Mod(_, _) =>
                Rem(lhs, rhs)
              case LessThan(_, _) =>
                Lt_s(lhs, rhs)
              case LessEquals(_, _) =>
                Le_s(lhs, rhs)
              case AmyAnd(_, _) =>
                And(lhs, rhs)
              case AmyOr(_, _) =>
                Or(lhs, rhs)
              case Equals(_, _) =>
                Eq(lhs, rhs)
              case Concat(_, _) =>
                Call(concatImpl.name, List(lhs, rhs))
            }
          case unaryOperator: UnaryOperator =>
            val e = cgExpr(unaryOperator.e)(false)
            unaryOperator match {
              case AmyNot(_) => Not(e)
              case AmyNeg(_) => Neg(e)
            }
          case Variable(name) => GetLocal(name)
          case IntLiteral(value) => Const(value)
          case BooleanLiteral(value) =>
            if (value) {
              True
            } else {
              False
            }
          case StringLiteral(value) => Strng(value)
          case UnitLiteral() => Unit
          case AmyCall(qname, args: List[Expr]) =>
            if (table.getFunction(qname).isDefined) {
              //function call
              val fun = table.getFunction(qname).get
              Call(fullName(fun.owner, qname), args.map(cgExpr(_)(false)))
            } else {
              ???
            }
        }
        if(ret) {
          Return(possibleReturnCode)
        } else {
          possibleReturnCode
        }

      case Sequence(e1: Expr, e2: Expr) =>
        Seq(cgExpr(e1)(false)) <:> cgExpr(e2)
      case Let(df, value: Expr, body: Expr) =>
        SetLocal(df.name, df.tt.tpe, cgExpr(value)(false)) <:> cgExpr(body)
      case Ite(cond: Expr, thenn: Expr, elze: Expr) =>
        If(cgExpr(cond)(false)) <:> cgExpr(thenn) <:> Else <:> cgExpr(elze) <:> End
      case Match(scrut: Expr, cases: List[MatchCase]) => {
        def matchAndBind(p: Pattern): (Code, Map[Identifier, Int]) = p match {
          case WildcardPattern() => (Const(1), Map())
          case IdPattern(name) => ???
            ///(SetLocal(newLoco) <:> Const(1) ,Map(name -> newLoco))
          case LiteralPattern(lit) => ???
            //cgExpr(lit) <:> Eq, Map())
          case CaseClassPattern(con, args) => ???
        }

        val scrutCode = cgExpr(scrut)
        val matchCode = cases.map(c => {
          val (code, newLocos) = matchAndBind(c.pat)
          code <:> cgExpr(c.expr) <:> Else
        })

        scrutCode <:> matchCode <:> Strng("Match error!") <:> cases.map(_ => End)
      }
      case Error(msg: Expr) =>
        Call("perror", List(cgExpr(msg)(false)), true) <:>
        Call("exit", List(GetLocal("EXIT_FAILURE")), true)
    }

    Module(
      program.modules.last.name.name,
      defaultIncludes,
      globalsNo,
      cFunctions ++ (program.modules flatMap cgModule)
    )

  }
}
