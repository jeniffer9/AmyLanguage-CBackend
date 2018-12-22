package amyc.test

import amyc.parsing._
import amyc.ast.{Identifier, SymbolicPrinter}
import amyc.ast.SymbolicTreeModule.Program
import amyc.analyzer.{NameAnalyzer, SymbolTable}
import amyc.utils._
import org.junit.Test
import scala.language.implicitConversions

class NameAnalyzerTests extends TestSuite {
  // A little hackery to overcome that Identifier names do not refresh over pipeline runs...
  private class TestUniquePrinter extends SymbolicPrinter {
    private val counter = new UniqueCounter[String]
    private val map = scala.collection.mutable.Map[Identifier, Int]()
    override implicit def printName(name: Identifier)(implicit printUniqueIds: Boolean): Document = {
      if (printUniqueIds) {
        val id = map.getOrElseUpdate(name, counter.next(name.name))
        s"${name.name}_$id"
      } else {
        name.name
      }
    }
  }

  private val treePrinterS: Pipeline[(Program, SymbolTable), Unit] = {
    new Pipeline[(Program, SymbolTable), Unit] {
      def run(ctx: Context)(v: (Program, SymbolTable)) = {
        println((new TestUniquePrinter)(v._1)(true))
      }
    }
  }

  val pipeline = Lexer andThen Parser andThen NameAnalyzer andThen treePrinterS

  val baseDir = "nameAnalyzer"

  val outputExt = "scala"

  @Test def testParamAndLocal = shouldOutput("ParamAndLocal")

  @Test def testArgumentNumberFunction = shouldFail("ArgumentNumberFunction")
}
