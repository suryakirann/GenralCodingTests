import scala.annotation.tailrec

/**
Problem Statement:
      “Build a full scala project runnable with sbt with tests (specs or scalatest) to show your ideas work.
      Write the following:

      1. Object that takes a generic list and can reverse it using a method that allows tail recursion. The test can
      implement an instance of any concrete object

      2. Object that takes a string of similar consecutive letters (all letters must be same case) and transforms this
      into a sequence consisting of the count of each consecutive letter followed by that letter. If a letter only
      appears once, do not apply a count. This should also demonstrate tail recursion. Also write a method that reverses
      this so that the output of the first method ran with this method will return the original result.
      e.g. given AAAAABBBCCC map this to 5A3B3C
      if you have some singles do not show "1", so AAAAABCDDD maps to 5ABC3D the reverse maps the right side to the left.”
 */

/**
 * Solution
 * Author: Venakta Nemani
 * Date: 06/03
 */

object TailRecursionCode extends App {

    @tailrec
    def tailRecursiveListReversal (l: List[Any], n: Int, acc: List[Any]): List[Any] = {
      if (n <= 0) acc
      else tailRecursiveListReversal(l, n-1, acc :+ l(n-1))
    }

  def countConsCharFrequency (str: String): String = {
    @tailrec
    def tailRecursiveCharFreqCount(nextChars: List[Char],
                                   currentChar: Char,
                                   count: Int,
                                   acc: String)
    : String = {
      nextChars match {
        case char :: xs if (char == currentChar) =>
          tailRecursiveCharFreqCount(
            nextChars = xs,
            currentChar,
            count + 1,
            acc
          )

        case char :: xs =>
          tailRecursiveCharFreqCount(
            nextChars = xs,
            currentChar = char,
            count = 1,
            s"$currentChar" + s"$count" + acc
          )

        case Nil =>
          tailRecursiveListReversal(
            (s"$currentChar" + s"$count" + acc).toList,
            (s"$currentChar" + s"$count" + acc).length,
            List())
            .filter(_ != '1')
            .mkString
      }
    }

    str.toList match {
      case char :: list =>
        tailRecursiveCharFreqCount(
          nextChars = list,
          currentChar = char,
          count = 1,
          acc = ""
        )

      case Nil =>
        ""
    }
  }

  println(countConsCharFrequency("AAAAABBBCCCD"))

}
