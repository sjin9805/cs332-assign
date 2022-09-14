package recfun
import common._

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
    if(c == 0 || c == r ) 1
    else pascal(c, r-1)+pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balancing(chars: List[Char], status: Int): Boolean = {
      if (status < 0) false
      else if (chars.isEmpty && status == 0) true
      else if (chars.head == "(") balancing(chars.tail, status+1)
      else if (chars.head == ")") balancing(chars.tail, status-1)
      else balancing(chars.tail, status)
    }
    balancing(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else if (money >= coins.head) countChange(money-coins.head,coins) + countChange(money, coins.tail)
    else countChange(money, coins.tail)
  }
}
