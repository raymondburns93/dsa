package sorting

object QuickSort {

  private def partition(arr: Array[Int], l: Int, r: Int): Int = {
    def swap(a: Int, b: Int) = {
      val tmp = arr(a)
      arr(a)  = arr(b)
      arr(b)  = tmp
    }

    var i = l - 1
    val x = arr(r)

    for (j <- l to r - 1)
      if (arr(j) <= x) {
        i = i + 1
        swap(i, j)
      }

    swap(i + 1, r)
    i + 1
  }

  // an imperative style QuickSort implementation
  def imperativeSort(arr: Array[Int]): Array[Int] = {
    def sort(arr: Array[Int], l: Int, r: Int): Array[Int] = {
      if (l >= r)
        arr
      else {
        val v = partition(arr, l, r)
        sort(arr, l, v - 1)
        sort(arr, v + 1, r)
      }
    }
    sort(arr, 0, arr.length - 1)
  }

  // a functional style QuickSort implementation
  def functionalSort(arr: Array[Int]): Array[Int] = {
    def sort(arr: Array[Int]): Array[Int] =
      if (arr.length < 2) arr
      else {
        val pivot = arr(arr.length / 2)
        sort(arr filter (pivot >)) ++ (arr filter (pivot ==)) ++ sort(arr filter (pivot <))
      }

    sort(arr)
  }

  def getExecutionTime(f: Array[Int] => Array[Int]): Array[Int] => Long = {
    args => {
      val st = System.currentTimeMillis
      f(args)
      val et = System.currentTimeMillis
      et - st
    }
  }

  def main(args: Array[String]): Unit = {

    val r = scala.util.Random
    val numbers = (for (i <- 1 to 100000) yield r.nextInt(10000)).toArray

    println("Time: " + getExecutionTime(imperativeSort)(numbers))
    println("Time: " + getExecutionTime(functionalSort)(numbers))

  }

}
