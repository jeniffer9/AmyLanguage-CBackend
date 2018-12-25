package amyc.codegenC

import java.io.{File, IOException}

import amyc.c.Module
import amyc.utils.{Context, Env, Pipeline}

// Prints file for c module
object CodePrinterC extends Pipeline[Module, Unit]{
  def run(ctx: Context)(m: Module) = {
    val outDirName = "cout"

    def withExt(ext: String) = s"$outDirName/${m.name}.$ext"

    val outDir = new File(outDirName)
    if (!outDir.exists()) {
      outDir.mkdir()
    }

    m.writeCText(withExt("c"))
  }
}
