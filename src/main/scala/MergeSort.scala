object MergeSort {

  def msort[T](compare: (T,T) => Boolean) (xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = {
      (xs,ys) match {
        case(_,Nil) => xs
        case(Nil,_) => ys
        case(xHead::xTail,yHead::yTail) => {
          if(compare(xHead,yHead)) xHead :: merge(xTail, ys)
          else yHead :: merge(xs, yTail)
        }
      }
    }
    val n = xs.length / 2
    if(n == 0) xs
    else {
      val(ys,zs) = xs.splitAt(n)
      merge(msort(compare)(ys),msort(compare)(zs))
    }
  }

}
