package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      b()*b() - 4*a()*c()    
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val d = delta()   
      if (d > 0) {
        val sqD = Math.sqrt(d)      
        Set((sqD-b())/(2.0*a()), (-sqD-b())/(2.0*a())) 
      } else if (d == 0) {
        Set(-1*b()/(2*a()))
      } else {
        Set()
      }
    }
  }
}
