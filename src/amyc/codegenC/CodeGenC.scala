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
      //StructType(owner +"_"+ fd.retType.tpe) else fd.retType.tpe

      Function(name, fd.params.map(f => new Parameter(f.name, f.tt.tpe, false, owner)), fd.retType.tpe) {
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
      AbstractClass(acd.name, table.getConstructorsForType(acd.name).get)
    }

    def cgCaseClass(cc: CaseClassDef, owner: Identifier): CaseClass = {
      val tpe = table.getConstructor(cc.name).get.retType
      val index = table.getConstructor(cc.name).get.index
      CaseClass(cc.name, owner, cc.fields.zipWithIndex.map { case (f, i) => new Parameter("param"+i, f.tt.tpe, false, owner) }, index, tpe.qname)
    }
    // Generate code for an expression expr.
    // Additional argument ret indicates a return is expected from the expression
    def cgExpr(expr: Expr, firstVoidLine: Boolean = false)(implicit ret: Boolean, module: String): Code = {
      val oneLiner = firstVoidLine && !ret && (expr match {
        case Sequence(_, _) => false
        case Match(_, _) => false
        case Error(_) => false
        case _ => true
      })
      if (oneLiner) {
        expr match {
          case Ite(cond, thenn, elze) => If(cgExpr(cond)(false, module)) <:> cgExpr(thenn, true) <:> Else <:> cgExpr(elze, true) <:> End
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
                  Call(qname.name + "_Constructor", args.map(cgExpr(_)(false, module)))
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
                SetLocal(df.name.name, df.tt.tpe, valueCode) <:> cgExpr(body)
              case _ =>
                SetLocal(df.name, df.tt.tpe, valueCode) <:> cgExpr(body)
            }
          case Ite(cond: Expr, thenn: Expr, elze: Expr) =>
            If(cgExpr(cond)(false, module)) <:> cgExpr(thenn) <:> Else <:> cgExpr(elze) <:> End
          case Match(scrut: Expr, cases: List[MatchCase]) => {

            def matchAndBind(p: Pattern, tpe: CType = CVoid)(implicit matchingCode: Code): (Code, List[Code], List[String]) = p match {
              case WildcardPattern() =>
                (True, List(), List())
              case IdPattern(name) =>
                if (tpe == CVoid)
                  (True, List(Define(GetLocal(name), matchingCode)), List(name))
                else
                  (True, List(SetLocal(name, tpe, matchingCode)), List())
              case LiteralPattern(lit) =>
                (Eq(matchingCode, cgExpr(lit)(false, module)), List(), List())
              case CaseClassPattern(con, args) => //matchingCode
                val c = table.getConstructor(con).get
                val params: List[(Code, List[Code], List[String])] = args.zipWithIndex.zip(c.argTypes).map(pa => matchAndBind(pa._1._1, pa._2)(GetProperty(Call("instance_"+con.name.toLowerCase, List(matchingCode)), GetLocal("param"+pa._1._2))))
                val paramsConditions: Code = if(params.size>0) params.map(_._1).reduceLeft(And(_, _)) else True
                val eBody = params.map(_._2).flatten
                (And(Eq(GetProperty(matchingCode, GetLocal("caseClass")), Const(c.index)), paramsConditions), eBody, params.flatMap(_._3))
            }

            val scrutCode = cgExpr(scrut)(false, module)
            val matchCode: List[Code] = (cases.zipWithIndex).map(c => {
              val (cond, body, toUndef) = matchAndBind(c._1.pat)(scrutCode)
              val undefs: Code = if(toUndef.size > 0) toUndef.map(c => i2c(Undefine(c))).reduceLeft(_ <:> _) else List[Code]()
              if (c._2 == 0)
                If(cond) <:> body <:> cgExpr(c._1.expr, true) <:> undefs
              else
                ElsIf(cond) <:> body <:> cgExpr(c._1.expr, true) <:> undefs
                //i2c(Case(cond, body <:> cgExpr(c.expr)))
            })
            matchCode.reduceLeft(_ <:> _) <:> Else <:> Call("perror", List("match error"), true) <:>
              Call("exit", List(GetLocal("EXIT_FAILURE")), true) <:>End
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
