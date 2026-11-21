package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (nextInt0, nextRng) = rng.nextInt
    val nextInt = if nextInt0 < 0 then math.abs(nextInt0 + 1) else nextInt0
    (nextInt, nextRng)

  def double(rng: RNG): (Double, RNG) =
    val (posInt, nextRng) = nonNegativeInt(rng)
    val double = posInt.toDouble / (Int.MaxValue + 1.0)
    (double, nextRng)

  val _double: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue + 1.0))

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (int, rng1) = rng.nextInt
    val (double, rng2) = RNG.double(rng1)
    ((int, double), rng2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((int, double), rng2) = intDouble(rng)
    ((double, int), rng2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @scala.annotation.tailrec
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) =
      if count <= 0 then (acc.reverse, rng)
      else
        val (nextInt, nextRng) = rng.nextInt
        go(count - 1, nextRng, nextInt :: acc)

    go(count, rng, List.empty[Int])

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      val (res, finalRng) = rs.foldLeft((List.empty[A], rng)) {
        case ((acc, nextRng), next) =>
          val (elem, nextRng2) = next(nextRng)

          (elem :: acc, nextRng2)
      }

      (res.reverse, finalRng)

  def sequenceViaMap2[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldLeft(unit(List.empty[A])):
      (acc, next) => map2(acc, next) { (ls, elem) => elem :: ls }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = ???

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      ???

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
