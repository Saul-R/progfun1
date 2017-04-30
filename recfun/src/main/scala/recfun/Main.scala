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
  def pascal(c: Int, r: Int): Int = 
    if (c <= 0 || c >= r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = {
    def balanceAux(open: Int, closed: Int, chars: List[Char]): Boolean = {
      if (open < closed) false
      else if (chars.isEmpty) open == closed
      else {
        val c = chars.head
        if (c.equals('('))
          balanceHelper(open + 1, closed, chars.tail)
        else if (c.equals(')'))
          balanceHelper(open, closed + 1, chars.tail)
        else
          balanceHelper(open, closed, chars.tail)
      }

    }
    balanceAux(0, 0, chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 1
    case (m, _) if m < 0 => 0
    case (_, cs) if cs.isEmpty => 0
    case (m, cs) => countChange(m - cs.head, cs) + countChange(m, cs.tail)
  }


  }
