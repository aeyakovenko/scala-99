import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scala99._

object P01_test extends Properties("P01") {
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
