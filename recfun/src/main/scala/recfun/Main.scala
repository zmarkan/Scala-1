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
  def pascal(c: Int, r: Int): Int =
    if(c == 0 || c == r ) 1
    else if(c < 0 || c > r) 0
    else (pascal(c-1, r-1) + pascal(c, r-1))


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    val closing = ')'
    val opening = '('

    def balanced(chars: List[Char], numOpening: Int): Boolean =
      if (numOpening < 0) false
      else if (chars.isEmpty) numOpening == 0
      else{
        if(chars.head == closing) balanced(chars.tail, numOpening -1)
        else if(chars.head == opening) balanced(chars.tail, numOpening + 1)
        else balanced(chars.tail, numOpening)
      }

    balanced(chars, 0)
  }

  /**
   * Exercise 3
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there are 3 ways to give change for 4 if you have coins with denomiation
   * 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countPosibilities(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if(money < 0) 0
      else if(coins.isEmpty && money > 0) 0
      else countPosibilities(money, coins.tail) +
        countPosibilities(money - coins.head, coins)

    countPosibilities(money, coins.sortWith(_.compareTo(_) < 0))
  }


  def and(x:Boolean, y:Boolean) = if(x){
    if(y) true; false
  } else false
}
