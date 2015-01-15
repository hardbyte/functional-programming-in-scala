package hardbyte

object arrayutil {
  def isSorted[A](a: Array[A], cmp: (A, A) => Boolean): Boolean = a.length match{
    case 0 => true
    case 1 => true
    case _ => cmp(a(0), a(1)) && isSorted(a.tail, cmp)
  }
}
