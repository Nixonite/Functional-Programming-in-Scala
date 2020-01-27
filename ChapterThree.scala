object ChapterThree {

  /* Exercise 3.2 */
  def tail[A](myList: List[A]): List[A] = myList match {
    case Nil => List()
    case x::xs => xs
  }

  /* Exercise 3.3 */
  def setHead[A](myList: List[A], newHead: A): List[A] = myList match {
    case Nil => List(newHead)
    case x::xs => newHead::xs
  }

  /* Exercise 3.4 */
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case x if (x < 0) => l
    case x => drop(ChapterThree.tail(l), n-1)
  }

  /* Exercise 3.5 */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => List()
    case List() => List()
    case List(x) => List(x)
    case x::xs if f(x) => dropWhile(xs, f)
    case _ => l
  }

  // Exercise 3.6
  



  /* Exercise 3.9 */
  def length[A](as: List[A]): Int = {
    as.foldRight(0)((a: A , b: Int) => 1 + b)
  }

  def main(args: Array[String]): Unit = {
    /*
      Exercise 3.1 - it's the second to last one, so the sum of elements
    */

    // Exercise 3.2
    println(ChapterThree.tail(List(1,2,3)))

    // Exercise 3.3
    println(ChapterThree.setHead(List(1,2,3), 42))

    // Exercise 3.4
    println(ChapterThree.drop(List(1,2,3,4,5), 3))
    
    // 3.5
    println(ChapterThree.dropWhile(List(1,3,3,4,5,6), (x: Int) => (x % 2 == 1)))

    // 3.6

    // Exercise 3.7
    // No because the function itself cannot break out of foldRight

    // 3.9
    println(ChapterThree.length(List(1,2,3,4)))
  }
}