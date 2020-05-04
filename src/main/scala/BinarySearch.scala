object BinarySearch extends App {

  def binarySearch(n: Int, a: Array[Int]): Int = {

    if (a.isEmpty) 999999999
    else {
      @scala.annotation.tailrec
      def go(lo: Int, hi: Int): Option[Int] = {
        if (lo > hi) None
        else {
          val mid: Int = lo + (hi - lo) / 2
          val midVal = a(mid)

          if (midVal == n) Some(mid)
          else if (midVal <= n) go(mid + 1, hi)
          else go(lo, mid - 1)
        }
      }

      go(0, a.length - 1).get
    }}

  println(binarySearch(17, Array(11, 12, 13, 14, 15, 16, 17, 18, 19, 20)))
}
