sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }
  
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(h,t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_,acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  // 3.11
  def my_sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => x + my_sum(xs)
  }

  // 3.11
  def my_prod(l: List[Int]): Int = l match {
    case Nil => 1
    case Cons(x, xs) => x * my_prod(xs)
  }

  // 3.11
  def my_foldleft_length[A](l: List[A]): Int = {
    foldLeft(l, 0)((x,_) => x + 1)
  }

  // 3.12
  def reverse_with_fold[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((x,y) => Cons(y,x))
  }

  // 3.14
  def append_with_foldLeft[A](l: List[A], e: A): List[A] = {
    foldRight(l, List[A](e))((x,y) => Cons(x,y))
  }

  // 3.16
  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((x: Int, y) => Cons(x + 1, y))
  }

  // 3.17
  def fold_to_string(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((x: Double, y) => Cons(x.toString, y))
  }

  // 3.18
  def my_map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((x: A, y) => Cons(f(x), y))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    
  }

  def verify[A](a: A, b: A): Unit = {
    println(a, b, a == b)
  }

  def main(args: Array[String]): Unit = {
    verify(my_sum(List(1,2,3)), 6)
    verify(my_prod(List(1,2,3)), 6)
    verify(List.foldLeft(List(1,2,3), 0)((x, _) => x + 1), 3)
    verify(reverse_with_fold(List(1,2,3)), List(3,2,1))
    verify(append_with_foldLeft(List(1,2,3), 4), List(1,2,3,4))
    verify(addOne(List(1,2,3)), List(2,3,4))
    verify(fold_to_string(List(3.2, 2.1)), List("3.2", "2.1"))
    verify(my_map(List(1,2,3))((x:Int) => x * 2), List(2,4,6))
  }
}