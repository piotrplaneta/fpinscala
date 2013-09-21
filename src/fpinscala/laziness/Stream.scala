package fpinscala.laziness

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
    foldRight(false)((a, acc) => p(a) || acc)

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

  def find(p: A => Boolean): Option[A] =
    filter(p).uncons.map(_.head)

  def unfoldMap[B](f: A => B): Stream[B] =
    Stream.unfold(this)(_.uncons.map(stream => (f(stream.head), stream.tail)))

  def unfoldTake(n: Int): Stream[A] = {
    Stream.unfold((this, n))(_ match {
      case(state, n) if n > 0 =>
        state.uncons.map(stream => (stream.head, (stream.tail, n - 1)))
      case _ => None
    })
  }

  def unfoldTakeWhile(f: A => Boolean): Stream[A] = {
    Stream.unfold((this))(_.uncons match {
      case Some(stream) if f(stream.head) => Some(stream.head, stream.tail)
      case (_) => None
    })
  }

  def unfoldZipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfoldZipAllWith1(s2)((_, _))

  def unfoldZipAllWith1[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    Stream.unfold(this, s2)(_ match {
      case (stream1, stream2) => (stream1.uncons, stream2.uncons) match {
        case (None, None) => None
        case (Some(xs), Some(ys)) => Some((f(Some(xs.head), Some(ys.head)), (xs.tail, ys.tail)))
        case (Some(xs), None) => Some((f(Some(xs.head), None), (xs.tail, Stream.empty[B])))
        case (None, Some(xs)) => Some((f(None, Some(xs.head)), (Stream.empty[A], xs.tail)))
      }
    })
  }

  def unfoldZipAllWith2[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    val s1WithNones = this map(x => Some(x)) append(Stream.constant(None))
    val s2WithNones = s2 map(x => Some(x)) append(Stream.constant(None))

    Stream.unfold(s1WithNones, s2WithNones)(_ match {
      case (stream1, stream2)
        if stream1.uncons.get.head.isEmpty && stream2.uncons.get.head.isEmpty => None
      case (stream1, stream2) => {
        val (h1, t1) = (stream1.uncons.get.head, stream1.uncons.get.tail)
        val (h2, t2) = (stream2.uncons.get.head, stream2.uncons.get.tail)

        Some((f(h1, h2), (t1, t2)))
      }
    })
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    s.unfoldZipAll(this).takeWhile(!_._1.isEmpty).
      forAll((pair) => pair._1 == pair._2)
  }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    this.tails exists (_.startsWith(s))

  def tails: Stream[Stream[A]] = {
    Stream.unfold(this)(_.uncons match {
      case Some(s) => Some((s, s.tail))
      case None => None
    }).append(Stream(Stream.empty[A]))
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] ={
    foldRight(Stream(z))((el, acc) =>
      Stream.cons(f(el, acc.uncons.get.head), acc))
  }
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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(prev: Int, current: Int): Stream[Int] =
      cons(prev, go(current, prev + current))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((x, state)) => cons(x, unfold(state)(f))
  }

  val unfoldOnes: Stream[Int] = unfold(1)(Some(1, _))

  def unfoldConstant[A](a: A): Stream[A] = unfold(a)(Some(a, _))

  def unfoldFrom(n: Int): Stream[Int] = unfold(n)(state => Some(state, state + 1))

  val unfoldFibs: Stream[Int] =
    unfold((0, 1))(state => Some(state._1, (state._2, state._1 + state._2)))
}
