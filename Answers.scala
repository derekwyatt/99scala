
import scala.annotation.tailrec
// Possibly "better" but way less readable
//
// def last[T](l: List[T]): T = {
//   if (l.isEmpty) throw new Exception("Can't get 'last' of Nil list")
//   l.foldLeft((l.size, l.head))((a, b) => if (a._1 == 1) (0, b) else ((a._1 - 1, b)))._2
// }
// 
// Better but probably not in the spirit of the question
//
// def last[T](l: List[T]): T = {
//   if (l.isEmpty) throw new Exception("Can't get 'last' of Nil list")
//   l.reverse.head
// }

// P01
@tailrec
def last[T](l: List[T]): T = {
  l match {
    case Nil => throw new Exception("Can't get 'last' of Nil list")
    case x :: Nil => x
    case x :: xs => last(xs)
  }
}

println(last(List(1,1,2,3,5,8)))

// P02
@tailrec
def penultimate[T](l: List[T]): T = {
  l match {
    case Nil => throw new Exception("Can't get 'penultimate' of Nil list")
    case x :: Nil => throw new Exception("Can't get 'penultimate' of list of size 1")
    case x :: y :: Nil => x
    case x :: y :: xs => penultimate(xs)
  }
}

println(penultimate(List(1,1,2,3,5,8)))

// P03
@tailrec
def nth[T](n: Int, l: Seq[T]): T = {
  if (n >= l.size) throw new Exception("Index out of bounds")
  n match {
    case 0 => l.head
    case _ => nth(n - 1, l.tail)
  }
}

println(nth(2, List(1,1,2,3,5,8)))

// P04
def length[T](l: Seq[T]): Int = {
  @tailrec
  def len[T](i: Int, s: Seq[T]): Int = {
    if (s.isEmpty) i
    else len(i + 1, s.tail)
  }
  len(0, l)
}

println(length(List(1,1,2,3,5,8)))

