import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scala.util.{Try,Success,Failure}
import scala99._

object Scala99Test extends Properties("scala99") {
  property("last") = forAll { (a: List[Int]) =>
    val x = Try(Scala99.last(a))
    val y = Try(a.last)
    (x,y) match {
      case (Success(x), Success(y)) => x == y
      case (Failure(x:NoSuchElementException), Failure(y:NoSuchElementException)) => true
      case _ => false
    }
  }

  property("penultimate") = forAll { (a: List[Int]) =>
    val x = Try(Scala99.penultimate(a))
    val y = Try(a.apply(a.length - 2))

    (x,y) match {
      case (Success(x), Success(y)) => x == y
      case (Failure(x:NoSuchElementException), Failure(y:NoSuchElementException)) => true
      case (Failure(x:NoSuchElementException), Failure(y:IndexOutOfBoundsException)) => true
      case _ => false
    }

  }

  property("nth") = forAll { (n : Int, a: List[Int]) =>
    val x = Try(Scala99.nth(n, a))
    val y = Try(a.apply(n))
    (x,y) match {
      case (Success(x), Success(y)) => x == y
      case (Failure(x:NoSuchElementException), Failure(y:NoSuchElementException)) => true
      case (Failure(x:NoSuchElementException), Failure(y:IndexOutOfBoundsException)) => true
      case (Failure(x:IndexOutOfBoundsException), Failure(y:IndexOutOfBoundsException)) => true
      case _ => false
    }
  }

  property("length") = forAll { (a: List[Int]) => Scala99.length(a) == a.length }

  property("reverse") = forAll { (a: List[Int]) => Scala99.reverse(a) == a.reverse }

  property("isPalindrome") = forAll { (a: List[Int]) =>
    Scala99.isPalindrome(a ++ a.reverse)
  }

  property("isPalindrome2") = forAll { (a: List[Int]) => a match {
      case Nil => true
      case a:List[Int] => Scala99.isPalindrome(a ++ a.reverse.tail)
    }
  }

  property("flatten") = forAll { a: List[Int] => 
    val deep: List[Any] = a.foldLeft(List[Any]())((z,a) => List(z) :: List(a))
    Scala99.flatten(deep) == a
  }

}
