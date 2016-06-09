package recfun

object Main {
  def main(args: Array[String]) {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }
    //    println(balance("(()())()()(()())".toList))
    println(countChange(6, List(1,2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r < 0 || r < 0 || c < 0) 0
    else if (c == 0) 1
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], numLeftParentheses: Int): Boolean = {
      if (chars.isEmpty) numLeftParentheses == 0
      else if (chars.head != '(' && chars.head != ')') balance(chars.tail, numLeftParentheses)
      else if (chars.head == ')') if (numLeftParentheses == 0) false else balance(chars.tail, numLeftParentheses - 1)
      else balance(chars.tail, numLeftParentheses + 1)
    }

    balance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty) 0
    else (0 to (money / coins.head)).map { i => if(i * coins.head == money) 1 else countChange(money - i * coins.head, coins.tail)}.reduce(_+_)
  }

  def countChangeOld(money: Int, coins: List[Int]): Int = {
    def getChange(money: Int): Set[List[Int]] = {
      if (money == 0) Set.empty[List[Int]]
      else {
        val validCoins = coins.filter(c => c <= money)
        if (validCoins.size == 0) Set.empty[List[Int]]
        else validCoins.flatMap(c => if(c==money) Set[List[Int]](List(c)) else getChange(money - c).map( x => (c :: x).sorted)).toSet
      }
    }
    getChange(money).size
  }
}
