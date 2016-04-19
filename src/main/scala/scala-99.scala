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

    def rleDirect[A](ls: List[A]): List[(Int,A)] = ls match {
      case Nil => Nil
      case ls => {
        val st = ls.takeWhile(_ == ls.head)
        val rest = ls.drop(st.length)
        (st.length, st.head) :: rleDirect(rest)
      }
    }
    def duplicate[A](ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case h :: tail => h :: h :: duplicate(tail)
    }
    def duplicateN[A](n: Int, ls: List[A]): List[A] = ls.map({ h => List.fill(n)(h) }).flatten 
    def groupByN[A](n: Int, ls: List[A]): List[List[A]] = ls match {
      case Nil => Nil
      case ls => { 
        val s = ls.take(n)
        val r = ls.drop(n)
        s :: groupByN(n, r) 
      }
    }
    def dropNth[A](n: Int, ls: List[A]): List[A] =
      groupByN(n, ls).map(_.take(n - 1)).flatten
    def split[A](n: Int, ls: List[A]): (List[A],List[A]) = (ls.take(n), ls.drop(n))
    def slice[A](j: Int, k: Int, ls: List[A]): (List[A],List[A],List[A]) =
      (ls.take(j), ls.drop(j).take(k - j), ls.drop(k))
    def rotate[A](n: Int, ls: List[A]): List[A] = (n,ls) match {
      case (_,Nil)  => Nil
      case (0,ls)   => ls
      case (n,ls)   => (ls ++ ls).drop((ls.length + (n % ls.length)) % ls.length).take(ls.length)
    }
    def deleteAt[A](n: Int, ls: List[A]): List[A] = (n,ls) match {
      case (_, Nil) => throw new IndexOutOfBoundsException
      case (0, ls) => ls.tail
      case (n, h :: tail) => h :: deleteAt(n - 1, tail)
    }
    def removeAt[A](n: Int, ls: List[A]): (List[A],A) = (deleteAt(n, ls), nth(n, ls))
    def insertAt[A](n: Int, e: A, ls: List[A]): List[A] = (n,ls) match {
      case (0, ls) => e :: ls
      case (_, Nil) => throw new IndexOutOfBoundsException 
      case (n, h :: tail) => h :: insertAt(n - 1, e, tail)
    }
  }
}
