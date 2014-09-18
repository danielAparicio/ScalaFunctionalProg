package week3

class Rational(x:Int,y:Int){

	//different between require and assert is that  is a precondition and assert
	//is used to check the function function code itself
	require(y!=0, "denominator must be nonzero")
	
	//second constructor that calls the other constructor, the implicit class constructor
	//in this case
	def this(x:Int)= this(x,1)
	
	//  private def gcd(a:Int, b:Int):Int = if (b==0) a else gcd(b, a % b)
	//  private val g = gcd(x,y)
	//	def numer = x/g
	//	def denom = y/g

	// this is another way to do it is always better to normalize (divide the numer and denom in this case)
	// the numbers as soon as possible to avoid stack overflows when performing operations
	private def gcd(a:Int, b:Int):Int = if (b==0) a else gcd(b, a % b)
	def numer = x
	def denom = y
	
	//In scala operatos can be used as identifiers x1 vector_++
	//this is another way to write less las looks better, we cand o the same with add,+ and sub,-
	def < (that:Rational) = this.numer * that.denom < that.numer * this.denom
	def less(that:Rational) = this.numer * that.denom < that.numer * this.denom
	
	def max(that:Rational) = if (this.less(that)) that else this
	
	def add(that:Rational)=
		new Rational (numer*that.denom + that.numer * denom, denom * that.denom)
	def + (that:Rational)=
		new Rational (numer*that.denom + that.numer * denom, denom * that.denom)
	
	//special convention in scala unary(IMP: we need the space between the end of the identifier and the colon) to do neg
	def unary_- : Rational = new Rational(-numer,denom)
	def neg : Rational = new Rational(-numer,denom)
	  
	def sub(that:Rational)= add(that.neg)
	// this is using - instead of sub, + instead of add and the unary -
	def - (that:Rational)= this + -that
			
		
	override def toString = {
		// wrong is better to normalize the numbers(divide by g ) as soon as posible
		val g = gcd(numer,denom)
		numer + "/" + denom
  }
}	