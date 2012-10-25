package functionalsets

object Main {
  def main(args: Array[String]) {
  }

  type Set = Int => Boolean
  def contains(s: Set, elem: Int): Boolean = s(elem)
}
