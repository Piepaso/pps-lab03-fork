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
    assertEquals(Cons(0, Cons(1 ,Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(Stream.takeWhile(stream)(_ < 5)))

  @Test def testFill(): Unit =
    val stream = Stream.iterate(0)(_ + 1)
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(Stream.fill(3)("a")))

  @Test def testFibonacci(): Unit =
    val fibonacci: Stream[Int] = Stream.fibonacci(0, 1)
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), Stream.toList(Stream.take(fibonacci)(5)))

  @Test def testInterleave(): Unit =
    val s1 = cons(0, cons(2, cons(4, empty())))
    val s2 = Stream.iterate(1)(_ + 2)

    assertEquals(
      Cons(0, Cons(1 ,Cons(2, Cons(3, Cons(4, Cons(5, Cons(7, Nil()))))))),
      Stream.toList(Stream.take(Stream.interleave(s1, s2))(7))
    )

  @Test def testCycle(): Unit =
    val lst = Cons("a", Cons("b", Cons("c", Nil())))
    assertEquals(
      Cons("a", Cons("b" ,Cons("c", Cons("a", Cons("b", Nil()))))),
      Stream.toList(Stream.take(Stream.cycle(lst))(5))
    )