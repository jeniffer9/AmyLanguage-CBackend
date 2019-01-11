package amyc
package c

// A C module
case class Module(name: String, imports: List[String], functions: List[Function], types: List[AbstractClass], classes: List[CaseClass]) {

  import java.io.{File, FileWriter}

  def writeCText(fileName: String) = {
    val fw = new FileWriter(new File(fileName))
    fw.write(ModulePrinter(this))
    fw.flush()
  }
}