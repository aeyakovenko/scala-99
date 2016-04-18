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
  property("group") = forAll { a: List[Int] => 
    Scala99.flatten(Scala99.group(a)) == a
  }
  property("compress") = forAll { a: List[Int] => 
    val dups = a.zip(a).map(_.productIterator.toList).flatten
    Scala99.compress(dups) == Scala99.compress(a)
  }
  property("rle") = forAll { a: List[Int] => 
    Scala99.unrle(Scala99.rle(a)) == a
  }
  property("rle2") = forAll { a: List[Int] => 
    val c = Scala99.compress(a)
    Scala99.rle(c) == List.fill(c.length)(1).zip(c)
  }
  property("rleDirect") = forAll { a: List[Int] => 
    Scala99.unrle(Scala99.rleDirect(a)) == a
  }
  property("duplicate") = forAll { a: List[Int] => 
    val dups = a.zip(a).map(_.productIterator.toList).flatten
    Scala99.duplicate(a) == dups
  }
  property("duplicateN") = forAll { (n: Int, a: List[Int]) => 
    val t = Math.min(5, Math.max(n, 1))
    val b = Scala99.compress(a)
    Scala99.rle(Scala99.duplicateN(t, b)) == List.fill(b.length)(t).zip(b) 
  }
  property("dropNth") = forAll { (a: List[Int]) => 
    Scala99.dropNth(1, a) == Nil &&
    Scala99.dropNth(2, List(1,2,3)) == List(1,3) &&
    Scala99.dropNth(3, List(1,2,3,4,5,6,7,8,9,0)) == List(1,2,4,5,7,8,0)
  }
  property("split") = forAll { (n: Int, a: List[Int]) => 
    val (f,s) = Scala99.split(n, a)
    f ++ s == a && (n < 0 || a.length < n || f.length == n)
  }
}
