object ChapterTwo {

	/* Implement isSorted, which checks whether an Array[A] is sorted according 
	 to a given comparison function:
	 def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
	 */

	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(mylist: Array[A]): Boolean = {
			mylist match {
				case Array() => true
				case Array(_) => true
				case Array(x, y) if ordered(x,y) => true
				case Array(x, y, _*) if ordered(x,y) => loop(mylist.tail)
				case _ => false
			}
		}
		loop(as)
	}

	/* Let’s look at another example, currying, which converts a 
	 function f of two arguments into a function of one argument that 
	 partially applies f. Here again there’s only one implementation 
	 that compiles. Write this implementation.
	*/

	def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
		(a: A) => ((b: B) => f(a,b))
	}

	/*
	Implement uncurry, which reverses the transformation of curry. 
	Note that since => associates to the right, A => (B => C) 
	can be written as A => B => C.
	*/

	def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
		(a: A, b: B) => f(a)(b)
	}

	/*
	Implement the higher-order function that composes two functions.
	*/

	def compose[A,B,C](f: B => C, g: A => B): A => C = {
		(a: A) => f ( g ( a ))
	}

	def main(args: Array[String]): Unit = {
		println(isSorted(Array(1,4,3,4,5), (x:Int, y:Int) => (x < y)))
		println(isSorted(Array("111", "212", "313", "414"), (x:String, y:String) => (x.toInt < y.toInt)))

		println(curry((x: Int, y: Int) => x + y)(3)(5))

		val f1 = (x:Int) => x + 2
		val g1 = (x:Int) => x + 3
		println(compose(f1, g1)(4))
	}
}
