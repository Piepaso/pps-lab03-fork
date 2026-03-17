package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Optionals.Optional.{Empty, Just}

class StreamTest:
  import u03.Streams.*
  import u03.Sequences.*
  import Stream.*
  import Sequence.*

  @Test def testTakeWhile(): Unit =
    val stream = Stream.iterate(0)(_ + 1)
    assertEquals(Cons (0 , Cons (1 , Cons (2 , Cons (3 , Cons (4 , Nil () ) ) ) ) ), Stream.toList(Stream.takeWhile(stream)(_ < 5)))

  @Test def testFill(): Unit =
    val stream = Stream.iterate(0)(_ + 1)
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(Stream.fill(3)("a")))

  @Test def testFibonacci(): Unit =
    val fibonacci: Stream[Int] = Stream.fibonacci(0, 1)
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), Stream.toList(Stream.take(fibonacci)(5)))