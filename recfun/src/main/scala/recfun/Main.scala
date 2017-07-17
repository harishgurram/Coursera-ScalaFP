package recfun

import scala.annotation.tailrec

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
      if(c==0 || c == r) 1 //break on edge columns
      else pascal(c-1, r-1) + pascal(c, r-1) //recurse to compute the other values in traingle
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec def balanceInternal(chars: List[Char], parens: List[Char]): Boolean =
      if (chars.isEmpty) parens.isEmpty // empty chars should have empty parens as well
      else if (chars.head == '(') balanceInternal(chars.tail, chars.head :: parens) //case of starting char is a open paren
      else if (chars.head != ')') balanceInternal(chars.tail, parens) // case for non closing praen on start
      else if (!parens.isEmpty) balanceInternal(chars.tail, parens.tail) //continue if parens is not yet empty
      else false // if we are here the parens are not yet balanced

    balanceInternal(chars, Nil) // start will nil parens
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1 //there is only 1 answer for no money case
      else if (money < 0 || coins.isEmpty) 0 // money < 0 or no denominations is not valid
      // recurse by to find all combination further deducting each denomination from money
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
