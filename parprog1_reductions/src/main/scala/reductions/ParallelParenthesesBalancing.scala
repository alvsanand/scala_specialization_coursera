package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var left = 0

    var result = true
    var i = 0
    while (result && i < chars.length) {
      if (chars(i) == '(') left = left + 1
      else if (chars(i) == ')' && left == 0) result = false
      else if (chars(i) == ')') left = left - 1

      i = i + 1
    }

    result && left == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, to: Int): (Int, Int) = {
      var left = 0
      var right = 0
      for(i <- from until to){
        if(chars(i)=='(') left = left + 1
        else if(chars(i)==')' && left > 0) left = left - 1
        else if(chars(i)==')') right = right + 1
      }
      (left, right)
    }

    def reduce(from: Int, to: Int): (Int, Int) = {
      if(to - from <= threshold)
        traverse(from, to)
      else{
        val halfDiff = (to-from) / 2
        val parResult = parallel(reduce(from, from + halfDiff), reduce(from + halfDiff, to))
        val balanced = Math.min(parResult._1._1, parResult._2._2)
        (parResult._1._1 - balanced + parResult._2._1, parResult._2._2 - balanced + parResult._1._2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
