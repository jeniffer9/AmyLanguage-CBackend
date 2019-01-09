object Fractionals {

  // Type definition of the Rational type
  abstract class Rational
  case class FractionalFull(num: Int, denom: Int, divisor: Int) extends Rational // private/internal
  case class Fractional(num: Int, denom: Int) extends Rational
  case class Number(n: Int) extends Rational

  // this method normalizes a fraction to its normalized form
  def normalize(f: Rational): Rational = {
    f match {
      case FractionalFull(_, _, 1) => f
      case FractionalFull(n, d, div) => FractionalFull(n / div, d / div, 1)
      case Fractional(n, d) =>
        val gcd: Int = gcd(n, d);
        FractionalFull(n / gcd, d / gcd, 1)
      case Number(n) => FractionalFull(n, 1, 1)
    }
  }

  // this function prints the rational number: a fraction
  def toString(f: Rational): String = {
    f match {
      case FractionalFull(n, 1, 1) => Std.intToString(n)
      case FractionalFull(n, 1, div) => toString( normalize(Fractional(n, div)) )
      case FractionalFull(n, d, 1) =>  Std.intToString(n) ++ " / " ++ Std.intToString(d)
      case FractionalFull(n, d, div) => toString(normalize(f))
      case Fractional(n,1) => Std.intToString(n)
      case Fractional(n,d) => toString(normalize(f))
      case Number(n) => Std.intToString(n)
    }
  }

  //--------------------------------------------------------------
  //        helper
  //--------------------------------------------------------------

  def absolute(x: Int): Int = {
    if(x < 0) {
      x*(-1)
    } else { x }
  }

  def gcd(x: Int, y: Int): Int = {
    val a: Int = absolute(x);
    val b: Int = absolute(y);

    if (a == 0 || b == 0) {
      a + b
    } else {
      if (a < b) {
        gcd(a, b % a)
      } else {
        gcd(a % b, b)
      }
    }
  }

  //--------------------------------------------------------------
  //        mathematical operations
  //--------------------------------------------------------------

  // (+, fraction, fraction)
  def plus(f1: Rational, f2: Rational): Rational = {
    f1 match {
      case FractionalFull(n1, d1, 1) => f2 match {
        case FractionalFull(n2, d2, 1) =>
          val gcd: Int = gcd(d1, d2);
          val leftM: Int = d2 / gcd;
          val rightM: Int = d1 / gcd;
          FractionalFull(n1 * leftM + n2 * rightM, d1 * leftM, 1)
        case _ => plus(f1, normalize(f2))
      }
      case FractionalFull(n1, d1, div) => f2 match { // all the cases below here could be simplified by decreasing efficiency a bit
        case FractionalFull(n2, d2, 1) => plus(normalize(f1), f2)
        case _ => plus(normalize(f1), normalize(f2))
      }
      case _ => f2 match {
        case FractionalFull(n2, d2, 1) => plus(normalize(f1), f2)
        case _ => plus(normalize(f1), normalize(f2))
      }
    }
  }

  // (-, fraction, fraction)
  def minus(f1: Rational, f2: Rational): Rational = { f2 match {
    case FractionalFull(n2, d2, 1) => plus(f1, FractionalFull(-n2, d2, 1))
    case FractionalFull(n2, d2, div) => plus(f1, FractionalFull(-n2, d2/div, 1))
    case _ => minus(f1, normalize(f2))
  }}

  // (*, fraction, fraction)
  def mul(f1: Rational, f2: Rational): Rational = {
    f1 match {
      case FractionalFull(n1, d1, 1) => f2 match {
        case FractionalFull(n2, d2, 1) => normalize(Fractional(n1*n2, d1*d2))
        case _ => mul(f1, normalize(f2))
      }
      case _ => mul(normalize(f1), normalize(f2))
    }
  }

  // (/, fraction, fraction)
  def div(f1: Rational, f2: Rational): Rational = {
    f1 match {
      case FractionalFull(n1, d1, 1) => f2 match {
        case FractionalFull(n2, d2, 1) => normalize(Fractional(n1*d2, n2*d2))
        case _ => mul(f1, normalize(f2))
      }
      case _ => mul(normalize(f1), normalize(f2))
    }
  }

  // (*, scalar, fraction) – scalar multiplication of a fraction
  def smul(x: Int, f: Rational): Rational = {
    f match {
      case FractionalFull(n, d, div) => normalize(Fractional(x * n, d / div)) // can be improved speed-wise
      case Fractional(n,d) => normalize(Fractional(x * n, d))
      case Number(n) => Number(x * n)
    }
  }

  //--------------------------------------------------------------
  //        comparison
  //--------------------------------------------------------------

  def equals(f1: Rational, f2: Rational): Boolean = {
    f1 match {
      case FractionalFull(n1, d1, 1) => f2 match {
        case FractionalFull(n2, d2, 1) => n1==n2 && d1==d2
        case _ => equals(f1, normalize(f2))
      }
      case _ => equals(normalize(f1), normalize(f2))
    }
  }

  def smaller(f1: Rational, f2: Rational): Boolean = {
    f1 match {
      case FractionalFull(n1, d1, 1) => f2 match {
        case FractionalFull(n2, d2, 1) => n1*d2 < n2*d1
        case _ => smaller(f1, normalize(f2))
      }
      case _ => smaller(normalize(f1), normalize(f2))
    }
  }

  def smallerEquals(f1: Rational, f2: Rational): Boolean = {
    f1 match {
      case FractionalFull(n1, d1, 1) => f2 match {
        case FractionalFull(n2, d2, 1) => n1*d2 <= n2*d1
        case _ => smallerEquals(f1, normalize(f2))
      }
      case _ => smallerEquals(normalize(f1), normalize(f2))
    }
  }

  //--------------------------------------------------------------
  //        examples & usage
  //--------------------------------------------------------------

  val frac: Rational = Fractional(10, 1);
  val frac1: Rational = Fractional(10, 3);
  val frac2: Rational = FractionalFull(10, 4, 2);
  val frac3: Rational = Fractional(3, 8);

  // addition & subtraction
  Std.printString("Addition & substraction:");
  val add1: Rational = plus(frac, frac1);
  val sub1: Rational = minus(smul(-1, frac), frac1);

  Std.printString(toString( add1 ) ++ " negation: " ++ toString( sub1 ));     // gcd = 1; 40/3

  // multiplication & divison
  Std.printString(""); Std.printString("Multiplication & division:");
  val mul1: Rational = mul(frac2, frac3);
  val mul2: Rational = smul(6, mul(frac2, frac3));
  val mul3: Rational = smul(3, frac1);

  Std.printString("6 * (" ++ toString(mul1) ++ ") = " ++ toString(mul2) );    // 6*((5/2) * (3/8)) = 6 * (15/16) = 45/8

  // comparisons
  Std.printString(""); Std.printString("Comparisons:");
  val comp: Boolean = equals(frac, mul3);
  val comp1: Boolean = smaller(frac1, frac);
  val comp2: Boolean = smaller(frac, frac);
  val comp3: Boolean = smallerEquals(frac, frac);

  Std.printString("3 * (" ++ toString(frac1) ++ ") = " ++ toString(frac) ++ ", is " ++ Std.booleanToString(comp));
  Std.printString(toString(frac1) ++ " < " ++ toString(frac) ++ ", is " ++ Std.booleanToString(comp1));
  Std.printString(toString(frac) ++ " < " ++ toString(frac) ++ ", is " ++ Std.booleanToString(comp2));
  Std.printString(toString(frac) ++ " <= " ++ toString(frac) ++ ", is " ++ Std.booleanToString(comp3));

  // print fractions
  Std.printString(""); Std.printString("printing out different fractions:");
  Std.printString(toString(frac));
  Std.printString(toString(frac2))
}
