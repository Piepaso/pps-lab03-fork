package it.unibo.pps.tasksPart2

import u03.Sequences.Sequence
import Sequence.*

import scala.annotation.tailrec

object es2:
  @tailrec
  def foldLeft[A, B](s: Sequence[A])(acc: B)(op: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(op(acc, h))(op)
    case _ => acc
