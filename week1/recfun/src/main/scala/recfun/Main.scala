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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanced(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) {
        open == 0
      } else {
        val headChar = chars.head                             // Head char
        val openedNo =                                        // Get opened number by +1/-1 if we see parentheses
          if (headChar == '(') open + 1
          else if (headChar == ')') open - 1
          else open

        if (openedNo >= 0) balanced(chars.tail, openedNo)
        else false                                            // If the openedNo falls below zero then unbalanced
      }
    }

    balanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1                                         // Base case: if money == 0, then we have found a count
    else if (money < 0 || coins.isEmpty) 0                    // If money is <0 or we run out of coins then no count
    else {
      countChange(money - coins.head, coins) +                // Count the ways using just the head
        countChange(money, coins.tail)                        // And count the ways using the tail
    }
  }
}
