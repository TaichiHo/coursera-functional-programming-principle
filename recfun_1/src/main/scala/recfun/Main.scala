package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def judge(chars: List[Char], l: Int, r: Int): Boolean = {
      if (chars.isEmpty) l == r
      else if (chars.head == '(') judge(chars.tail, l + 1, r)
      else if (chars.head == ')') {
        if (r + 1 > l) false
        else judge(chars.tail, l, r + 1)
      }
      else judge(chars.tail, l, r)
    }
    judge(chars, 0, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def dfs(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) {
        if (money == 0) 1 else 0
      }
      else {
        val options = (0 to money / coins.head).toArray
        val count = options.foldLeft(0)((x, y) => x + dfs(money - y * coins.head, coins.tail))
        count
      }
    }
    if (money == 0 || coins.isEmpty) 0
    else dfs(money, coins)
  }
}
