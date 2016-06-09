def general(f1: Int => Int, f2: (Int, Int)=>Int , defValue: Int)(a: Int, b: Int): Int =
  if (a > b) defValue else f2(f1(a), general(f1, f2, defValue)(a + 1, b))

def factorial(n: Int): Int =
  general(x=>x, (a,b)=>a*b, 1)(1,n)
def sum(n: Int): Int =
  general(x=>x, (a,b)=>a+b, 0)(1,n)

factorial(5)
sum(5)

class Rational(private val n: Int, private val d: Int) {
  require(d != 0, "y cannot be 0")
  private def gcd(a:Int, b:Int):Int = if(b==0) a else gcd(b, a % b)

  private val g = gcd(n, d)

  val x = n / g
  val y = d / g

  def +(r: Rational): Rational = {
    new Rational(x * r.y + r.x * y , y * r.y)
  }

  override def toString = if(y==1) s"$x" else s"$x / $y"
}

(new Rational(4,2)) + (new Rational(1,2))

new Rational(4,0)


