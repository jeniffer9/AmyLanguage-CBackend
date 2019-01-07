object UnitTest {

  def unit() : String = {
    val s: String = Std.readString();
    s
  }

  Std.printString("UnitTest: " ++ unit())
}