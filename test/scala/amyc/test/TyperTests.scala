package amyc.test

import amyc.parsing._
import amyc.ast.{Identifier, SymbolicPrinter}
import amyc.ast.SymbolicTreeModule.Program
import amyc.analyzer.{NameAnalyzer, TypeChecker}
import amyc.utils._
import org.junit.Test

class TyperTests extends TestSuite {
  // We need a unit pipeline
  private def unit[A]: Pipeline[A, Unit] = {
    new Pipeline[A, Unit] {
      def run(ctx: Context)(v: A) = ()
    }
  }

  val pipeline = Lexer andThen Parser andThen NameAnalyzer andThen TypeChecker andThen unit

  val baseDir = "typer"

  val outputExt = "" // No output files for typechecking

  @Test def testArithError1 = shouldFail("ArithError1")

  @Test def testArithmetic = shouldPass("Arithmetic")
}
