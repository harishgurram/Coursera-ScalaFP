package greeter

object SampleWorksheet {
    val x = 1                                     //> x  : Int = 1
  def increase(i: Int) = i + 1                    //> increase: (i: Int)Int
  increase(x)                                     //> res0: Int = 2
  
  def product(f: Int =>Int)(a:Int, b:Int):Int = {
  	if (a>b) 1
  	else f(a) * product(f)(a+1,b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
  product(x=>x)(2, 4)                             //> res1: Int = 24
  
  def fact(n: Int) = product(x=>x)(1, n)          //> fact: (n: Int)Int
  fact(5)                                         //> res2: Int = 120
  
  type Set = Int => Boolean
  
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: greeter.SampleWorksheet.Set, elem: Int)Boolean
}

class rational(x:Int, y:Int){

	def nume= x
	def den=y
	

}

object rationals {
def r = new rational(4,5)


}