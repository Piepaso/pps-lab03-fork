package it.unibo.pps.tasksPart2

import org.junit.Test
import es1.*
import es2.*
import es3.*
import it.unibo.pps.u02.AlgebraicDataTypes.Person
import Person.*
import org.junit.Assert.*
import u03.Sequences.*
import Sequence.*


class tasksPart2Test:

  val persons = Cons(Teacher("Aguzzi", "PPS"), Cons(Student("Pasini", 2022),
    Cons(Teacher("Ricci", "PCD"), Cons(Teacher("Viroli", "PPS"), Nil()))))
  val numbers = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  def coursesTester(coursesFun: Sequence[Person] => Sequence[String]): Unit =
    assertEquals(Cons("PPS", Cons("PCD", Cons("PPS", Nil()))), coursesFun(persons))

  @Test def testCourses(): Unit = coursesTester(courses)

  @Test def testFlatMapCourses(): Unit = coursesTester(flatMapCourses)

  @Test def testFoldLeft(): Unit = assertEquals(-16, foldLeft(numbers)(0)(_ - _))

  @Test def testCountDistinctCourses(): Unit = assertEquals(2, countDistinctCourses(persons))
