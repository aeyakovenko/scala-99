import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scala99._

object Scala99Test extends Properties("scala99") {

  def last[A](ls: List[A]): A = ls.last
  property("islast") = forAll { (a: List[Int]) =>
    a match {
      case Nil =>
       try {
         Scala99.last(a)
         false
       } catch {
         case ex: NoSuchElementException => true
         case ex: Throwable => false
       }
      case _ => Scala99.last(a) == last(a)
    }
  }
  def penultimate[A](ls: List[A]): A = ls.apply(ls.length - 2)
  property("penultimate") = forAll { (a: List[Int]) =>
    (a.length <= 1) match {
      case true =>
       try {
         Scala99.penultimate(a)
         false
       } catch {
         case ex: NoSuchElementException => true
         case ex: Throwable => false
       }
      case _ => Scala99.penultimate(a) == penultimate(a)
    }
  }
  property("nth") = forAll { (n : Int, a: List[Int]) =>
    (a.length <= n || n < 0) match {
      case true => 
       try {
         Scala99.nth(n, a)
         false
       } catch {
         case ex: NoSuchElementException => true
         case ex: IndexOutOfBoundsException => true
         case ex: Throwable => false
       }
      case _ => Scala99.nth(n, a) == a.apply(n)
    }
  }
}
