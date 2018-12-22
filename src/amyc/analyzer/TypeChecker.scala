package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        
        case Equals(lhs, rhs) =>
          //lhs has to have the same type as rhs but it's generic
          val st = TypeVariable.fresh()
          genConstraints(lhs, st) ++ genConstraints(rhs, st) ++ topLevelConstraint(BooleanType)
        
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) = pat match
          {
            case CaseClassPattern(constr, args) =>
              val c = table.getConstructor(constr).get
              val constraint = Constraint(c.retType, scrutExpected, pat.position)
              if (args.length > 0) {
                val (cs, mp) = args.zip(c.argTypes).map(n => handlePattern(n._1, n._2)) reduce ((a, b) => (a._1 ++ b._1, a._2 ++ b._2))
                (constraint :: cs, mp)
              } else {
                //if no arguments => no additional bindings
                (List(constraint), Map())
              }
            case WildcardPattern() =>
              // fresh type for wildCard
              val st = TypeVariable.fresh()
              //make new identifier such that wildCard can be invoked
              (List(Constraint(st, scrutExpected, pat.position)), Map((Identifier.fresh("_"), st)))
            case IdPattern(name) =>
              //no idea if this is correct
              val st = TypeVariable.fresh()
              (List(Constraint(st, scrutExpected, pat.position)), Map((name, st)))
            case LiteralPattern(lit) =>
              //no additional bindings
              (genConstraints(lit, scrutExpected), Map())
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        case Variable(name) =>
          topLevelConstraint(env.get(name).get)
        case Plus(lhs,rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Minus(lhs, rhs)  =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Times(lhs, rhs)  =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Div(lhs, rhs)  =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Mod(lhs, rhs)  =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case LessThan(lhs, rhs)  =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)
        case LessEquals(lhs, rhs)  =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)
        case And(lhs, rhs)  =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)
        case Or(lhs, rhs)  =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)
        case Concat(lhs, rhs)  =>
          genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType) ++ topLevelConstraint(StringType)
        case Not(e) =>
          genConstraints(e, BooleanType) ++ topLevelConstraint(BooleanType)
        case Neg(e) =>
          genConstraints(e, IntType) ++ topLevelConstraint(IntType)
        case Call(name, exprs) =>
          //name analyzer has make sure that call exists
          val c = table.getFunction(name).getOrElse(table.getConstructor(name).get)
          exprs.zip(c.argTypes).flatMap(n => genConstraints(n._1, n._2)) ++ topLevelConstraint(c.retType)
        case Sequence(e1, e2) =>
          genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, expected)
        case Let(fd@ParamDef(name, tt), v, b) =>
          genConstraints(v, tt.tpe) ++ genConstraints(b, expected)(env + ((name, tt.tpe)))
        case Ite(cond, thenn, elze) =>
          genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)
        case Error(msg) => //Error is any type
          genConstraints(msg, StringType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case StringLiteral(_) =>
          topLevelConstraint(StringType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more => (found, expected) match {
          case (TypeVariable(id1), TypeVariable(id2)) =>
            if (id1 != id2)
              solveConstraints(subst_*(more, id2, found))
            else
              solveConstraints(more)
          case (_, TypeVariable(id)) =>
            solveConstraints(subst_*(more, id, found))
            //orient
          case (TypeVariable(_), _) =>
            solveConstraints(Constraint(expected, found, pos) :: more)
          case (ClassType(qn1), ClassType(qn2)) =>
            if (qn1 != qn2)
              error(s"typeError: $qn1 is not of type $qn2", pos)
            else
              solveConstraints(more)
          case _ =>
            if (found.getClass() != expected.getClass())
              error(s"typeError: $found is not of type $expected", pos)
            else
              solveConstraints(more)
        }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
