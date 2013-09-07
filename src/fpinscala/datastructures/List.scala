package fpinscala.datastructures

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => {
      if (n == 1) xs
      else drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs)(f)
      else Cons(x, xs)
    }
  }

  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => 1 + y)

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]) =
    foldLeft(l, 0.0)(_ + _)

  def product3(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => x + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x, y) => f(y, x))

  def reverse2[A](l: List[A]): List[A] =
    foldLeft2(l, Nil: List[A])((x, y) => Cons(y, x))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  def concat[A](xsList: List[List[A]]): List[A] =
    foldRight(xsList, Nil:List[A])((xs, ys) => append2(xs, ys))

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, addOne(xs))
  }

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[A])((x, acc) => Cons(f(x), acc))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f)
    }
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    val filterFunction = (x: A) => {
      if (f(x)) List(x)
      else Nil
    }

    flatMap(l)(filterFunction)
  }

  def zipAdd(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipAdd(xs, ys))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs))
  }
}
