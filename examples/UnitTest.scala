object UnitTest {

  abstract class Bullshit
  case class BullshitBig(index: Int) extends Bullshit

  val bs: Bullshit = BullshitBig(2);
  2
}