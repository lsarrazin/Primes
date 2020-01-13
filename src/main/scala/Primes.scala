// Scala 2.13.1

/**
 * This exercise is about :
 *   - Given N (positive natural)
 *   - What is the minimal P (positive natural) that is dividable by all naturals between [1 to N] (included)
 *
 */

object Primes extends App {

  /**
   * Produces the natural sequence starting at n
   * @param n
   * @return
   */
  def numberStream(n: BigInt): LazyList[BigInt] = {
    def loop(v: BigInt): LazyList[BigInt] = v #:: loop(v + 1)
    loop(n)
  }

  /**
   * Produces all primes from the given natural stream
   * @param stream
   * @return
   */
  def Eratosthenes(stream: LazyList[BigInt]): LazyList[BigInt] =
    stream.head #:: Eratosthenes((stream.tail) filter (x => x % stream.head != 0))

  /**
   * Primes stream
   */
  val primes = Eratosthenes(numberStream(2))

  /**
   * Decompose a number to its primes
   * @param n
   * @return
   */
  def asPrimes(n: BigInt): LazyList[BigInt] =
    if (n <= 1) LazyList(1)
    else {
      val prime = primes.takeWhile(_ < n/2).filter(n % _ == 0).take(1)
      if (prime.isEmpty) LazyList(n)
      else prime ++ asPrimes(n / prime.head)
    }

  /**
   * Decompose a number to the minimal subset of primes
   */
  def ComputePrimeSet(n: BigInt): LazyList[BigInt] =
      numberStream(1).takeWhile(_ <= n)
        .foldLeft(LazyList.empty[BigInt])(
          (knownPrimes: LazyList[BigInt], i: BigInt) => {
            val newPrimes: LazyList[BigInt] = asPrimes(i).diff(knownPrimes)
            knownPrimes ++ newPrimes
          }
        )

  // And that's it ...
  (1 to 100).map { n =>
    val ps = ComputePrimeSet(n)
    println(n + " => " + ps.fold(BigInt(1))(_*_))
  }
}
