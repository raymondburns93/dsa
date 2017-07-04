package sorting

object MergeSort {

  private def merge(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y)
          x :: merge(xs1, ys)
        else
          y :: merge(xs, ys1)
    }

  def mergeSort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (left, right) = xs splitAt (n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers = List(3,2,1,4)
    println(mergeSort(numbers))
  }

}
