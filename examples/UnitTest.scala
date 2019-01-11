object UnitTest {

  abstract class List
  case class Nil() extends List
  case class Cons(head: Int, tail: List) extends List

  val l: List = Cons(1, Cons(2, Cons(2, Nil())));
  l match {
    case Nil() => 1
    case Cons(h, _) => 2
  }
}