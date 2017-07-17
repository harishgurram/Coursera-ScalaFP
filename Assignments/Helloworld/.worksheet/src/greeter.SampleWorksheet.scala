package greeter

object SampleWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(55); 
    val x = 1;System.out.println("""x  : Int = """ + $show(x ));$skip(31); 
  def increase(i: Int) = i + 1;System.out.println("""increase: (i: Int)Int""");$skip(14); val res$0 = 
  increase(x);System.out.println("""res0: Int = """ + $show(res$0));$skip(104); 
  
  def product(f: Int =>Int)(a:Int, b:Int):Int = {
  	if (a>b) 1
  	else f(a) * product(f)(a+1,b)
  };System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(22); val res$1 = 
  product(x=>x)(2, 4);System.out.println("""res1: Int = """ + $show(res$1));$skip(44); 
  
  def fact(n: Int) = product(x=>x)(1, n);System.out.println("""fact: (n: Int)Int""");$skip(10); val res$2 = 
  fact(5)
  
  type Set = Int => Boolean;System.out.println("""res2: Int = """ + $show(res$2));$skip(87); 
  
  def contains(s: Set, elem: Int): Boolean = s(elem);System.out.println("""contains: (s: greeter.SampleWorksheet.Set, elem: Int)Boolean""")}
}

class rational(x:Int, y:Int){

	def nume= x
	def den=y
	

}

object rationals {
def r = new rational(4,5)


}
