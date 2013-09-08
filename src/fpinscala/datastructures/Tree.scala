package fpinscala.datastructures

import runtime.IntRef

/**
 * Created with IntelliJ IDEA.
 * User: piotrplaneta
 * Date: 9/8/13
 * Time: 6:08 PM
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + 1 + size(right)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A])(f: (A => B)): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(leafFunction: (A => B))(branchFunction: ((B, B) => B)): B = t match {
    case Leaf(value) => leafFunction(value)
    case Branch(left, right) =>
      branchFunction(fold(left)(leafFunction)(branchFunction), fold(right)(leafFunction)(branchFunction))
  }

  def foldSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((leftSize, rightSize) => leftSize + 1 + rightSize)

  def foldMax(t: Tree[Int]): Int =
    fold(t)((x: Int) => x )((leftMax, rightMax) => leftMax max rightMax)

  def foldDepth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((leftDepth, rightDepth) => 1 + (leftDepth max rightDepth))

  def foldMap[A, B](t: Tree[A])(f: (A => B)): Tree[B] =
    fold(t)(value => Leaf(f(value)): Tree[B])((left, right) => Branch(left, right))
}
