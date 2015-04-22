package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }
  
  test("computeDelta") {
    val delta1 = Polynomial.computeDelta(Var(1),Var(1),Var(1))
    assert(delta1() == -3)    
    val delta2 = Polynomial.computeDelta(Var(1),Var(2),Var(1))
    assert(delta2() == 0)
    val delta3 = Polynomial.computeDelta(Var(1),Var(-2),Var(1))
    assert(delta3() == 0)
    val delta4 = Polynomial.computeDelta(Var(1),Var(2),Var(2))
    assert(delta4() == -4)
    val delta5 = Polynomial.computeDelta(Var(3),Var(2),Var(3))
    assert(delta5() == -32)
  }
  
  
  test("computeSolutions") {
    val set1 = Polynomial.computeSolutions(Var(1),Var(2),Var(1), Polynomial.computeDelta(Var(1),Var(2),Var(1)))
    assert(set1() == Set(-1.0))
    
    val set2 = Polynomial.computeSolutions(Var(1),Var(-4),Var(4), Polynomial.computeDelta(Var(1),Var(-4),Var(4)))
    assert(set2() == Set(2.0))
    
    val set3 = Polynomial.computeSolutions(Var(1),Var(-5),Var(6), Polynomial.computeDelta(Var(1),Var(-5),Var(6)))
    assert(set3() == Set(2.0, 3.0))
    
    val set4 = Polynomial.computeSolutions(Var(1),Var(-1),Var(-6), Polynomial.computeDelta(Var(1),Var(-1),Var(-6)))
    assert(set4() == Set(-2.0, 3.0))
    
    val set5 = Polynomial.computeSolutions(Var(1),Var(1),Var(1), Polynomial.computeDelta(Var(1),Var(1),Var(1)))
    assert(set5() == Set())
   
  }
  
  def getExprFromSignalOption(key: String, map: Map[String, Signal[Double]]): Double = {
    map.get(key) match {
      case Some(value) => value()
      case None => Double.NaN
    }               
  }
  
  test("Calculator") {
    val map1 = Calculator.computeValues(Map("a"-> Signal(Literal(1.0))))
    assert(getExprFromSignalOption("a", map1) == 1.0)
    
    val map2 = Calculator.computeValues(Map("a"-> Signal(Plus(Literal(1.0), Literal(2.0)))))
    assert(getExprFromSignalOption("a", map2) == 3.0)
    
    val map3 = Calculator.computeValues(Map("a"-> Signal(Minus(Literal(5.0), Literal(1.0)))))
    assert(getExprFromSignalOption("a", map3) == 4.0)
    
    val map4 = Calculator.computeValues(Map("a"-> Signal(Times(Literal(3.0), Literal(2.0)))))
    assert(getExprFromSignalOption("a", map4) == 6.0)
    
    val map5 = Calculator.computeValues(Map("a"-> Signal(Divide(Literal(8.0), Literal(2.0)))))
    assert(getExprFromSignalOption("a", map5) == 4.0)
    
    val map6 = Calculator.computeValues(Map("a"-> Signal(Ref("b")), "b"-> Signal(Literal(5.0)) ))
    assert(getExprFromSignalOption("a", map6) == 5.0)
    
    val map7 = Calculator.computeValues(Map("a"-> Signal(Plus(Literal(3.0), Ref("b"))), "b"-> Signal(Literal(4.0)) ))
    assert(getExprFromSignalOption("a", map7) == 7.0)
    
  }

}

