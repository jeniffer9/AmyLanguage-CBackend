object TestCaseClass {

  abstract class Animal
  case class Dog(name: String) extends Animal

  def unit(x : Int) : Unit = {
    x;
    ()
  }

  val dog: Animal = Dog("fluffy");
  val s: String = dog;
  s
}