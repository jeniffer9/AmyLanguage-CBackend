package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }

    modNames.keys.toList foreach table.addModule


    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    p.modules.foreach { case mod => //know now that there are no duplicates
      mod.defs.groupBy(_.name) foreach { case (name, defs) =>
          if (defs.size > 1) {
            fatal(s"Two definitions named $name in module ${mod.name}", defs.head.position)
          }
        }
    }

    // Step 3: Discover types and add them to symbol table
    p.modules.foreach { case mod =>
      mod.defs foreach {
        case N.AbstractClassDef(name) => table.addType(mod.name, name)
        case _ =>
      }
    }

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach { case mod =>
      mod.defs foreach {
        case c@N.CaseClassDef(name, fields, parent) =>
          val par = table.getType(mod.name, parent)
          if (par == None) {
            fatal(s"Must extend type from same module", c.position)
          }
          table.addConstructor(mod.name, name, fields.map(n => transformType(n, mod.name)), par.get)
        case _ =>
      }
    }

    // Step 5: Discover functions signatures, add them to table
    p.modules.foreach { case mod =>
      mod.defs foreach {
        case N.FunDef(name, params, retType, _) =>
          val paramNames = params.groupBy(_.name)
          paramNames foreach { case (nameP, para) =>
            if (para.size > 1) {
              fatal(s"$nameP encountered multiple times in parameters list of function $name", para.head.position)
            }
          }
          table.addFunction(mod.name, name, params.map(p => transformType(p.tt, mod.name)), transformType(retType, mod.name))
        case _ =>
      }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    
    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case N.AbstractClassDef(name) =>
        val Some(sym) = table.getType(module, name)
        S.AbstractClassDef(sym)
      case N.CaseClassDef(name, fields, _) =>
        val Some((sym, sig)) = table.getConstructor(module, name)
        val newArgs = fields zip sig.argTypes map { case (tt, tpe) =>
          S.TypeTree(tpe).setPos(tt)
        }
        S.CaseClassDef(sym, newArgs, sig.parent)
      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }}.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)


      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = pat match {
            case N.CaseClassPattern(constr, args) =>
              val c = table.getConstructor(constr.module.getOrElse(module), constr.name)
              if (c == None) {
                fatal(s"Undefined Class", pat)
              }
              val Some((sym, sig)) = c
              if (sig.argTypes.length != args.length) {
                fatal(s"Not the right number of arguments", pat)
              }
              if(args.length > 0) {
                val newPatterns = args map transformPattern

                val lists = newPatterns map (_._2) reduce (_ ++ _)

                lists.groupBy(_._1) foreach { case (name, idfs) =>
                  if (idfs.size > 1) {
                    fatal(s"Two variables named $name in case ${constr.toString}", pat)
                  }
                }
                (S.CaseClassPattern(sym, newPatterns.map(_._1)).setPos(p), lists)
              } else {
                (S.CaseClassPattern(sym, List()).setPos(p), List())
              }

            case N.WildcardPattern() => (S.WildcardPattern().setPos(pat), List())
            case N.IdPattern(name) =>
              val c = table.getConstructor(module, name)
              if(c != None && c.get._2.argTypes.length == 0) {
                warning(s"Did you mean to match the case class $name() ?", pat)
              }
              if (locals.get(name) != None) {
                fatal(s"Variable $name has already been declared", pat)
              }
              val s = Identifier.fresh(name)
              (S.IdPattern(s).setPos(p), List((name, s)))
            case N.LiteralPattern(lit) =>
              val l = lit match {
                case N.IntLiteral(v) => S.IntLiteral(v)
                case N.BooleanLiteral(v) => S.BooleanLiteral(v)
                case N.StringLiteral(v) => S.StringLiteral(v)
                case N.UnitLiteral() => S.UnitLiteral()
              }
              (S.LiteralPattern(l.setPos(lit)).setPos(pat), List())
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            S.MatchCase(newPat, transformExpr(rhs)(module, (params, locals ++ moreLocals)))
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

        case N.Variable(name) =>
          val (locos, pars) = (locals.get(name), params.get(name))
          if (locos == None && pars == None) {
            fatal(s"The variable $name is not defined", expr)
          }
          S.Variable(locos.getOrElse(pars.get))
        case N.Plus(lhs,rhs) => S.Plus(transformExpr(lhs), transformExpr(rhs))
        case N.Minus(lhs, rhs)  => S.Minus(transformExpr(lhs), transformExpr(rhs))
        case N.Times(lhs, rhs)  => S.Times(transformExpr(lhs), transformExpr(rhs))
        case N.Div(lhs, rhs)  => S.Div(transformExpr(lhs), transformExpr(rhs))
        case N.Mod(lhs, rhs)  => S.Mod(transformExpr(lhs), transformExpr(rhs))
        case N.LessThan(lhs, rhs)  => S.LessThan(transformExpr(lhs), transformExpr(rhs))
        case N.LessEquals(lhs, rhs)  => S.LessEquals(transformExpr(lhs), transformExpr(rhs))
        case N.And(lhs, rhs)  => S.And(transformExpr(lhs), transformExpr(rhs))
        case N.Or(lhs, rhs)  => S.Or(transformExpr(lhs), transformExpr(rhs))
        case N.Equals(lhs, rhs)  => S.Equals(transformExpr(lhs), transformExpr(rhs))
        case N.Concat(lhs, rhs)  => S.Concat(transformExpr(lhs), transformExpr(rhs))
        case N.Not(e) => S.Not(transformExpr(e))
        case N.Neg(e) => S.Neg(transformExpr(e))
        case N.Call(name, exprs) =>
          val func = table.getFunction(name.module.getOrElse(module), name.name)
          val constr = table.getConstructor(name.module.getOrElse(module), name.name)
          if (func == None && constr == None) {
            fatal(s"function or constructor ${name.toString} cannot be found", expr)
          }
          val (sym, sig) = func.getOrElse(constr.get)
          if (sig.argTypes.length != exprs.length) {
            fatal(s"Call to ${name.toString} has wrong number of parameters", expr)
          }
          S.Call(sym, exprs map transformExpr)
        case N.Sequence(e1, e2) => S.Sequence(transformExpr(e1), transformExpr(e2))
        case N.Let(fd@N.ParamDef(name, tt), v, b) =>
          if(locals.get(name) != None) {
            fatal(s"The local variable $name is already defined", fd)
          }
          if(params.get(name) != None) {
            warning(s"The local variable will shadow the parameter $name")
          }
          val s = Identifier.fresh(name)
          S.Let(S.ParamDef(s, S.TypeTree(transformType(tt, module)).setPos(tt)).setPos(fd), transformExpr(v), transformExpr(b)(module, (params, locals + (name -> s))))
        case N.Ite(cond, thenn, elze) => S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        case N.Error(msg) => S.Error(transformExpr(msg))
        case N.IntLiteral(v) => S.IntLiteral(v)
        case N.BooleanLiteral(v) => S.BooleanLiteral(v)
        case N.StringLiteral(v) => S.StringLiteral(v)
        case N.UnitLiteral() => S.UnitLiteral()
      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
