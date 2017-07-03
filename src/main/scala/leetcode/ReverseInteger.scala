package leetcode

object ReverseInteger {

  def reverse(x: Int): Int = {
    def loop(rev: Int, y: Int): Int = {
      if (rev > Int.MaxValue || rev < Int.MinValue) return 0
      if (y == 0) return rev

      loop(rev * 10 + y % 10, y / 10)
    }

    loop(0, x)
  }

  def main(args: Array[String]): Unit = {
    println(reverse(321))
  }

}
