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
  }
}
