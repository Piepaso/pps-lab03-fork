package it.unibo.pps.tasksPart3

import u03.*
import Sequences.Sequence
import Streams.Stream

object tasksPart3:
  def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] =
    s
    