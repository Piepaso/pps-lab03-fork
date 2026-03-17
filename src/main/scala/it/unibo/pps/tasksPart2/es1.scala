package it.unibo.pps.tasksPart2

import it.unibo.pps.u02.AlgebraicDataTypes.Person
import u03.Sequences.Sequence
import Person.*
import Sequence.*

object es1:

  def isTeacher(p: Person): Boolean = p match
    case Teacher(_, _) => true
    case _ => false

  def getCourse(p: Person): String = p match
    case Teacher(n, c) => c

  def courses(s: Sequence[Person]): Sequence[String] =
    map(filter(s)(isTeacher))(getCourse)

  def flatMapCourses(s: Sequence[Person]): Sequence[String] =
    flatMap(s){ case Teacher(n, c) => Cons(c, Nil()); case _ => Nil() }