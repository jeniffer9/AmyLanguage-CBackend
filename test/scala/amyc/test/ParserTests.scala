package amyc.test

import amyc.parsing._
import org.junit.Test

class ParserTests extends TestSuite with amyc.MainHelpers {
  val pipeline = Lexer andThen Parser andThen treePrinterN("")

  val baseDir = "parser"

  val outputExt = "scala"

  @Test def testEmpty = shouldOutput("Empty")
  @Test def testLiterals = shouldOutput("Literals")

  @Test def testEmptyFile = shouldFail("EmptyFile")
}

