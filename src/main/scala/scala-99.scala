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
  }
}
