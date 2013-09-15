package fpinscala.errorhandling

/**
 * Created with IntelliJ IDEA.
 * User: piotrplaneta
 * Date: 9/9/13
 * Time: 10:34 PM
 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(_ => this).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(x => if (f(x)) Some(x) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  import java.util.regex._

  def pattern(s: String): Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches) // The details of this API don't matter too much, but `p.matcher(s).matches` will check if the string `s` matches the pattern `p`.

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f =>
      mkMatcher(pat2) map     (g =>
        f(s) && g(s)))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x <- a
      y <- b
    } yield(f(x, y))
  }

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((matcher1, matcher2) => matcher1(s) && matcher2(s))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])((el, listInOption) =>
      map2(el, listInOption)( _ :: _)
    )
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil): Option[List[B]])((el, listInOption) =>
      map2(f(el), listInOption)(_ :: _)
    )
  }
}
