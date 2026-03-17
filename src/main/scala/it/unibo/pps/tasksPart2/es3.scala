package it.unibo.pps.tasksPart2

import es1.*
import es2.*
import it.unibo.pps.u02.AlgebraicDataTypes.Person
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{distinct, filter, map}

object es3:
  def countDistinctCourses(s: Sequence[Person]): Int =
    foldLeft(distinct(map(filter(s)(isTeacher))(getCourse)))(0)((a, e) => a + 1)