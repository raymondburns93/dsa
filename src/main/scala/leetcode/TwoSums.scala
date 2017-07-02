package leetcode

import scala.collection.mutable

object TwoSums {

  // O(n^2) implementation
  def twoSumsImpl1(nums: Array[Int], target: Int): Array[Int] = {

    for (i <- 0 to nums.length - 1)
      for (j <- i + 1 to nums.length - 1)
        if (nums(i) + nums(j) == target)
          return Array(i, j)

    Array()
  }

  // O(n) implementation
  def twoSumsImpl2(nums: Array[Int], target: Int): Array[Int] = {
    val hm = new mutable.HashMap[Int, Int]()

    @annotation.tailrec
    def loop(i: Int): Array[Int] = {
      if (i == nums.length) return Array()

      hm.get(target - nums(i)) match {
        case Some(x) => return Array(x, i)
        case None    => { hm += ((nums(i), i)); loop(i + 1) }
      }

    }
    loop(0)
  }

  def getExecutionTime(f: (Array[Int], Int) => Array[Int]): (Array[Int], Int) => Long = {
    (arr, num) => {
      val st = System.currentTimeMillis
      f(arr, num)
      val et = System.currentTimeMillis
      et - st
    }
  }

  def main(args: Array[String]): Unit = {

    val arr    = (1 to 1000000).toArray
    var target = 1002444

    println("Time: " + getExecutionTime(twoSumsImpl1)(arr, target))
    println("Time: " + getExecutionTime(twoSumsImpl2)(arr, target))
  }
}
