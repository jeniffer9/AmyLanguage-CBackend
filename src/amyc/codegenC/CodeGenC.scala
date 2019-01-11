package amyc
package codegenC

import analyzer._
import ast.SymbolicTreeModule.{And => AmyAnd, Call => AmyCall, Div => AmyDiv, Neg => AmyNeg, Not => AmyNot, Or => AmyOr, _}
import ast.Identifier
import c.{CaseClass, Instructions, _}
import Instructions._
import UtilsC._
import utils.{Context, Pipeline}

import scala.language.implicitConversions

// Generates C code for an Amy program
object CodeGenC extends Pipeline[(Program, SymbolTable), Module] {

  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v

    def cgTypes(moduleDef: ModuleDef): List[AbstractClass] = {
      val ModuleDef(name, defs, _) = moduleDef
      // Generate code for all functions
      defs.collect {
        case acd: AbstractClassDef => cgAbstractClass(acd, name)
      }
    }

    def cgClasses(moduleDef: ModuleDef): List[CaseClass] = {
      val ModuleDef(name, defs, _) = moduleDef
      // Generate code for all functions
      defs.collect {
        case cc: CaseClassDef => cgCaseClass(cc, name)
      }
    }

    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect {
        case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) => cgFunction(fd, name, false)
      } ++
        // Generate code for the "main" function, which contains the module expression
      //FIXME handle case of no main
        optExpr.toList.map { expr =>
          val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
          cgFunction(mainFd, name, true)
        }
    }

    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the c function name from a combination of
      // module and function name, since we put everything in the same c file.
      val name = if(isMain) "main" else fullName(owner, fd.name)
      Function(name, fd.params, fd.retType.tpe) {
        val body = cgExpr(fd.body, fd.retType.tpe == UnitType)(ret = !(isMain || fd.retType.tpe == UnitType), owner)
        if (isMain) {
          val (front, last) = body.instructions.splitAt(body.instructions.size-1)
          front <:> Seq(last) <:> Return(Const(0))
        } else {
          body
        }
      }
    }

    def cgAbstractClass(acd: AbstractClassDef, owner: Identifier): AbstractClass = {
      val name = fullName(owner, acd.name)
      AbstractClass(name, table.getConstructorsForType(acd.name).get)
    }

    def cgCaseClass(cc: CaseClassDef, owner: Identifier): CaseClass = {
      val name = fullName(owner, cc.name)
      val tpe = table.getConstructor(cc.name).get.retType
      val index = table.getConstructor(cc.name).get.index
      CaseClass(name, owner, cc.fields.zipWithIndex.map{case (f,i) => new Parameter("field" + i, f.tpe, false, owner)}, index, tpe.qname)
    }

    // Generate code for an expression expr.
    // Additional argument ret indicates a return is expected from the expression
    def cgExpr(expr: Expr, firstVoidLine: Boolean = false)(implicit ret: Boolean, module: String): Code = {
      val oneLiner = firstVoidLine && (expr match {
        case Sequence(_, _) => false
        case _ => true
      })
      if (oneLiner) {
        expr match {
          case Ite(cond, thenn, elze) => If(cgExpr(cond)) <:> cgExpr(thenn, true) <:> Else <:> cgExpr(elze, true) <:> End
          case _ => OneLiner(cgExpr(expr))
        }
      } else {
        expr match {
          case retType: RetType =>
            val possibleReturnCode = retType match {
              case binaryOperator: BinaryOperator =>
                val lhs = cgExpr(binaryOperator.lhs)(false, module)
                val rhs = cgExpr(binaryOperator.rhs)(false, module)
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
                val e = cgExpr(unaryOperator.e)(false, module)
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
                  Call(fullName(fun.owner, qname), args.map(cgExpr(_)(false, module)))
                } else {
                  Call(fullName(module, qname.name + "_Constructor"), args.map(cgExpr(_)(false, module)))
                }
            }
            if (ret) {
              Return(possibleReturnCode)
            } else {
              possibleReturnCode
            }

          case Sequence(e1: Expr, e2: Expr) =>
            Seq(cgExpr(e1)(false, module)) <:> cgExpr(e2)
          case Let(df, value: Expr, body: Expr) =>
            val valueCode = cgExpr(value)(false, module)
            valueCode.instructions.head match {
              case Call(name, _, _) if (name == "Std_readString") =>
                SetLocal(df.name, df.tt.tpe, AllocateMem(Const(5000))) <:> Call("scanf", List("%s", GetLocal(df.name)), true) <:> cgExpr(body)
              case Call(name, _, _) if (name == "Std_readInt") =>
                SetLocal(df.name, df.tt.tpe, Const(0)) <:> Call("scanf", List("%d", GetLocal("&"+df.name)), true) <:> cgExpr(body)
              case Call(name, _, _) if (name.endsWith("Constructor")) =>
                SetLocal(df.name, StructType(module+"_"+df.tt.tpe), valueCode) <:> cgExpr(body)
              case _ =>
                SetLocal(df.name, df.tt.tpe, valueCode) <:> cgExpr(body)
            }
          case Ite(cond: Expr, thenn: Expr, elze: Expr) =>
            If(cgExpr(cond)(false, module)) <:> cgExpr(thenn) <:> Else <:> cgExpr(elze) <:> End
          case Match(scrut: Expr, cases: List[MatchCase]) => {
            def matchAndBind(p: Pattern): Code = p match {
              case WildcardPattern() => Const(1)
              case IdPattern(name) => ???
              ///(SetLocal(newLoco) <:> Const(1) ,Map(name -> newLoco))
              case LiteralPattern(lit) => ???
              //cgExpr(lit) <:> Eq, Map())
              case CaseClassPattern(con, args) => Case(GetLocal(con.name.toUpperCase))

            }

            val scrutCode = cgExpr(scrut)
            val matchCode = cases.map(c => {
              val code = matchAndBind(c.pat)
                code
            })
            Switch(scrutCode, matchCode)
          }
          case Error(msg: Expr) =>
            Call("perror", List(cgExpr(msg)(false, module)), true) <:>
              Call("exit", List(GetLocal("EXIT_FAILURE")), true)
        }
      }
    }

    Module(
      program.modules.last.name.name,
      defaultIncludes,
      cFunctions ++ (program.modules flatMap cgModule),
      program.modules flatMap cgTypes,
      program.modules flatMap cgClasses
    )

  }
}
