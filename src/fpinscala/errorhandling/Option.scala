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