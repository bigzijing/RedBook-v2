package fpinscala.exercises.datastructures

import fpinscala.answers.iomonad.IO1.lines

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = 
    l match
      case Nil => sys.error("Tail of empty list")
      case Cons(head, tail) => tail 
    
  def setHead[A](l: List[A], h: A): List[A] = 
    l match
      case Cons(head, tail) => Cons(h, tail)
      case _ => sys.error("Cannot setHead on empty list")

  def drop[A](l: List[A], n: Int): List[A] = 
    if n <= 0 then l 
    else l match 
      case Cons(head, tail) => drop(tail, n - 1)
      case Nil => Nil

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l

  def init[A](l: List[A]): List[A] = 
    l match
      case Cons(head, Cons(tail, Nil)) => Cons(head, Nil) 
      case Cons(head, tail) => Cons(head, init(tail))
      case Nil => sys.error("Cannot call init on empty list")
    
  def length[A](l: List[A]): Int = 
    foldRight(l, 0, (_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = 
    l match
      case Nil => acc
      case Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def sumViaFoldLeft(ns: List[Int]): Int = 
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = 
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = 
    foldLeft(l, 0, (acc, next) => acc + 1)

  def reverse[A](l: List[A]): List[A] = 
    foldLeft[A, List[A]](l, Nil, (acc, next) => Cons(next, acc))

  def foldRightViaFoldLeft[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def foldRightViaFoldLeft2[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    // foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B
    foldLeft(
      l = as,
      acc = (b: B) => b,
      f = (g, a) => b => g(f(a, b))
    )(acc)

  def foldLeftViaFoldRight[A,B](l: List[A], acc: B, f: (B, A) => B): B = 
    foldRight(reverse(l), acc, (a, b) => f(b, a))

  def foldLeftViaFoldRight2[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    foldRight(l, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
    foldRight(l, r, (next, acc) => Cons(next, acc))

  def concat[A](l: List[List[A]]): List[A] = 
    foldLeft(l, Nil: List[A], (acc, next) => appendViaFoldRight(acc, next))

  def incrementEach(l: List[Int]): List[Int] = 
    foldRight(l, Nil: List[Int], (next, acc) => Cons(next + 1, acc))

  def doubleToString(l: List[Double]): List[String] = 
    foldRight(l, Nil: List[String], (next, acc) => Cons(next.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] = 
    foldRight(l, Nil: List[B], (next, acc) => Cons(f(next), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] = 
    foldRight(as, Nil: List[A], (next, acc) => if f(next) then Cons(next, acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = 
    foldRight(as, Nil: List[B], (next, acc) => appendViaFoldRight(f(next), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = 
    flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = 
    (a, b) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
      case _ => Nil
    
  def zipWith[A, B, C](ls1: List[A], ls2: List[B], f: (A, B) => C): List[C] = 
    (ls1, ls2) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
      case _ => Nil 

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = 
    def subsequenceMatch(sup: List[A], sub: List[A]): Boolean = 
      (sup, sub) match  
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => subsequenceMatch(t1, t2)
        case _ => false

    (sup, sub) match 
      case (Nil, _) => sub == Nil
      case (Cons(h1, t1), Cons(h2, _)) if h1 == h2 => 
        if subsequenceMatch(sup, sub) then true else hasSubsequence(t1, sub)
      case (Cons(_, t1), _) => hasSubsequence(t1, sub)