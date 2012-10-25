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
    if(c <= 0 || r == c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def b(waiting: Int, ch: List[Char]): Boolean = {
      ch match {
        case head :: tail if head == '(' => b(waiting + 1, tail)
        case head :: tail if head == ')' && waiting > 0 => b(waiting - 1, tail)
        case head :: tail if head == ')' => b(waiting - 2, tail)
        case head :: tail => b(waiting, tail)
        case Nil => waiting == 0
      }
    }
    b(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (_, Nil) => 0
      case (money, coins) if((coins.size == 1) && (money % coins.head == 0)) => 1
      case (money, coins) => {
        if(money >= coins.head) {
          val possibilities = (money / coins.head)
          def countWithOthers(p: Int, count: Int): Int = {
            if(p <= possibilities) {
              val c = countChange(money - (coins.head * p), coins.tail)
              countWithOthers(p+1, count + c)
            } else count
          }
          countChange(money, coins.tail) + countWithOthers(1, 0)
        } else {
          countChange(money, coins.tail)
        }
      }
    }
  }
}
