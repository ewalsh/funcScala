package ai.economicdatasciences.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
  }

  def tail[A](xs: List[A]): List[A] = {
      xs match {
          case Nil => Nil
          case Cons(_, x) => x
      }
  }

  def setHead[A](xs: List[A], newhead: A): List[A] = {
      xs match {
          case Nil => Nil
          case Cons(_, x) => Cons(newhead, x)
      }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
      if(n <= 0) l
      else l match {
          case Nil => Nil
          case Cons(_, t) => drop(t, n-1)
      }
  }

  // def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  //     case Cons(h, t) if f(h) => dropWhile(t, f)
  //     case _ => l
  // }

  // using a curried setup let's the function know A type in function
  // is the same as in List
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = {
      as match {
          case Cons(h, t) if f(h) => dropWhile(t)(f)
          case _ => as
      }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
      a1 match {
          case Nil => a2
          case Cons(h, t) => Cons(h, append(t, a2))
      }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
          case Nil => z
          case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
  }

  def sum2(ns: List[Int]): Int = {
      foldRight(ns, 0)((x, y) => x + y)
  }

  def product2(ns: List[Double]): Double = {
      foldRight(ns, 1.0)((x,y) => x * y)
  }

  foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y)
}
