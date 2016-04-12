import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

// P01 (*) Find the last element of a list.
//     Example:
//     scala> last(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 8

// The start of the definition of last should be
//     def last[A](l: List[A]): A = ...
// The `[A]` allows us to handle lists of any type.

object P01 {
  // There are several ways to solve this problem.  If we use builtins, it's very
  // easy.
  def lastBuiltin[A](ls: List[A]): A = ls.last

  // The standard functional approach is to recurse down the list until we hit
  // the end.  Scala's pattern matching makes this easy.
  def lastRecursive[A](ls: List[A]): A = ls match {
    case h :: Nil  => h
    case _ :: tail => lastRecursive(tail)
    case _         => throw new NoSuchElementException
  }
}

object P01_Tests extends Properties("P01") {
  property("islast") = forAll { (a: List[Int]) =>
    a match {
      case Nil =>
       try {
         P01.lastRecursive(a)
         false
       } catch {
         case ex: NoSuchElementException => true
         case ex: Throwable => false
       }
      case _ => P01.lastBuiltin(a) == P01.lastRecursive(a)
    }
  }
}
