package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Checking parenthesis")
    println(balance("(())".toList))
    println(balance(")(()))".toList))
    println(balance("(()())".toList))

    println("Checking count")
    println(countChange(2, List()))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if ((c < 0) || (c > r)) 0
    else if (r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def checkParenthesis(text: List[Char], openParenthesis: Int): Boolean =
      if (openParenthesis < 0) false
      else if (text.isEmpty) true
      else {
        val char = text.head
        if (char == ')') checkParenthesis(text.tail, openParenthesis - 1)
        else if (char == '(') checkParenthesis(text.tail, openParenthesis + 1)
        else checkParenthesis(text.tail, openParenthesis)
      }

    checkParenthesis(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money == 0) 1
    else if (money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
