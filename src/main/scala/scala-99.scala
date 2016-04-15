package scala99 {
  object Scala99 {
    def last[A](ls: List[A]): A = ls match {
      case h :: Nil  => h
      case _ :: tail => last(tail)
      case _         => throw new NoSuchElementException
    }

    def penultimate[A](ls: List[A]): A = ls match {
      case h :: Nil       => throw new NoSuchElementException
      case h :: _ :: Nil  => h
      case _ :: tail      => penultimate(tail)
      case _              => throw new NoSuchElementException
    }

    def nth[A](n:Int, ls: List[A]): A = (n < 0) match {
      case true => throw new IndexOutOfBoundsException 
      case _    => ls.drop(n).head
    }

    def length[A](ls: List[A]): Int = ls.map(x => 1).foldLeft(0) (_+_)

    def flip[A,B,C](f: (A,B)=>C): ((B,A)=>C) = (b,a) => f(a,b)

    def reverse[A](ls: List[A]): List[A] = ls.foldLeft(List[A]())(flip(_::_))

    def isPalindrome[A](ls: List[A]): Boolean = reverse(ls) == ls

    def flatten[A](ls: List[Any]): List[Any] = ls.flatMap {
       case as:List[_] => flatten(as)
       case a => List(a)
    }

    def group[A](ls: List[A]): List[List[A]] = ls match {
      case Nil => Nil
      case ls => {
        val st = ls.takeWhile(_ == ls.head)
        val rest = ls.drop(st.length)
        st :: group(rest)
      }
    }
    def compress[A](ls: List[A]): List[A] = group(ls).map(_.head)
    def rle[A](ls: List[A]): List[(Int,A)] = group(ls).map({ ls => (ls.length, ls.head)})
    def unrle[A](ls: List[(Int,A)]): List[A] = {
      val f = (a: Int, b: A) => List.fill(a)(b)
      ls.map(f.tupled).flatten
    }
    def rleFast[A](ls: List[A]): List[(Int,A)] = ls.foldLeft(List[(Int,A)]()) { (z,h) => 
      z match {
        case Nil => (1, h) :: Nil
        case ((c,v)::rest) => if(h == v) (c+1,v)::rest else (1,h)::z
      }
    }.reverse
  }
}
