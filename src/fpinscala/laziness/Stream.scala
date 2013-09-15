package fpinscala.laziness

/**
 * Created with IntelliJ IDEA.
 * User: piotrplaneta
 * Date: 9/15/13
 * Time: 1:01 PM
 */
sealed trait Stream[+A] {
  def uncons: Option[Cons[A]]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case None => Nil
    case Some(s) => s.head :: s.tail.toList
  }

  def exists(p: A => Boolean): Boolean = uncons match {
    case Some(s) => p(s.head) || s.tail.exists(p)
    case None => false
  }

  def take(n: Int): Stream[A] = uncons match {
    case Some(s) if n > 0 => Stream.cons(s.head, s.tail.take(n - 1))
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some(s) if p(s.head) => Stream.cons(s.head, s.tail.takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
    case Some(c) => f(c.head, c.tail.foldRight(z)(f))
    case None => z
  }

  def existsWithFold(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFold(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((x, acc) => {
      if (p(x)) Stream.cons(x, acc)
      else Empty
    })
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((x, acc) => Stream.cons(f(x), acc))

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((x, acc) => {
      if (p(x)) Stream.cons(x, acc)
      else acc
    })
  }

  def append[B >: A](s2: Stream[B]): Stream[B] =
    foldRight(s2)((x, acc) => Stream.cons(x, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((x, acc) => f(x).append(acc))
}

object Empty extends Stream[Nothing] {
  val uncons = None
}

sealed abstract class Cons[+A] extends Stream[A] {
  def head: A
  def tail: Stream[A]
  val uncons = Some(this)
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = hd
    lazy val tail = tl
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
}