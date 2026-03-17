package u03

import u03.Optionals.Optional

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
      case (_, n) if n == 0 => s
      case (Cons(_, t), _) => skip(t)(n - 1)
      case _ => Nil()


    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()

    def tailRecZip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      @tailrec
      def z(s1: Sequence[A], s2: Sequence[B], acc: Sequence[(A, B)]): Sequence[(A, B)] = (s1, s2, acc) match
        case (Cons(h1, t1), Cons(h2, t2), _) => z(t1, t2, Cons((h1, h2), acc))
        case _ => reverse(acc)

      z(first, second, Nil())

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Cons(h1, t1), _) => Cons(h1, concat(t1, s2))
      case (Nil(), Cons(h2, t2)) => Cons(h2, concat(Nil(), t2))
      case _ => Nil()

    def tailRecConcat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] =
      @tailrec
      def c(s1: Sequence[A], s2: Sequence[A], acc: Sequence[A]): Sequence[A] = (s1, s2, acc) match
        case (Cons(h1, t1), _, _) => c(t1, s2, Cons(h1, acc))
        case (Nil(), Cons(h2, t2), _) => c(Nil(), t2, Cons(h2, acc))
        case _ => reverse(acc)

      c(s1, s2, Nil())

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def tailRecReverse(s: Sequence[A], reversed: Sequence[A]): Sequence[A] = (s, reversed) match
        case (Cons(h, t), _ ) => tailRecReverse(t, Cons(h, reversed))
        case _ => reversed

      tailRecReverse(s, Nil())

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      @tailrec
      def tailRecFlatMap(s: Sequence[A], acc: Sequence[B]): Sequence[B] = (s, acc) match
        case (Cons(h, t), _) => tailRecFlatMap( t, tailRecConcat(acc, mapper(h)))
        case _ => acc

      tailRecFlatMap(s, Nil())

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] =
      import Optional.*
      @tailrec
      def tailRecMin(s: Sequence[Int], minimum: Optional[Int]): Optional[Int] = (s, minimum) match
        case (Cons(h, t), Just(m)) if h >= m => tailRecMin(t, Just(m))
        case (Cons(h, t), _) => tailRecMin(t, Just(h))
        case _ => minimum

      tailRecMin(s, Empty())


    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, Cons(_, tt)) => Cons(h, evenIndices(tt))
      case _ => s

    def tailRecEvenIndices[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def alternate(s: Sequence[A], acc: Sequence[A], take: Boolean): Sequence[A] = (s, acc, take) match
        case (Cons(h, t), acc, true) =>  alternate(t, Cons(h, acc), false)
        case (Cons(h, t), acc, false) => alternate(t, acc, true)
        case _ => reverse(acc)
      
      alternate(s, Nil(), true)

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean =(s, elem) match
      case (Cons(h, t), _) if h == elem => true
      case (Cons(h, t), _) => contains(t)(elem)
      case _ => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def compare(s: Sequence[A], res: Sequence[A]): Sequence[A] = (s, res) match
        case (Cons(h, t), _) if !contains(res)(h) => compare(t, Cons(h, res))
        case (Cons(h, t), _) => compare(t, res)
        case _ => reverse(res)

      compare(s, Nil())

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      @tailrec
      def tailRecGroup(s: Sequence[A], acc: Sequence[Sequence[A]]): Sequence[Sequence[A]] = (s, acc) match
        case (Cons(h, t), Cons(Cons(ahh, aht), at)) if h == ahh => tailRecGroup(t, Cons(Cons(h, Cons(ahh, aht)), at))
        case (Cons(h, t), acc) => tailRecGroup(t, Cons(Cons(h, Nil()), acc))
        case _ => reverse(acc)
      tailRecGroup(s, Nil())


    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @tailrec
      def tailRecPartition(s: Sequence[A], acc: (Sequence[A], Sequence[A])): (Sequence[A], Sequence[A]) = (s, acc) match
        case (Cons(h, t), (left, right)) if pred(h) => tailRecPartition(t, (Cons(h, left), right))
        case (Cons(h, t), (left, right)) => tailRecPartition(t, (left, Cons(h, right)))
        case (_, (left, right)) => (reverse(left), reverse(right))

      tailRecPartition(s, (Nil(), Nil()))



@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
