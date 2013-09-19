package com.faacets
package ext

import scala.math._
import alg._
import spire.std.int._
import spire.math.{lcm, Rational}

/** Extensions of correlation inequalities with binary outputs, from 
  * Wu et al. arXiv:1302.6698 Compact Bell inequalities for multipartite experiments.
  */
object Wu2013 {
  import alg.immutable.QVector
  object Example1 {
    val s = Scenario(Vector(Vector(2,2,2),Vector(2,2,2)))
    val b1 = Bra(s, NCRepr, QVector(-2,0,0,0,  0,0,1,-1,  0,0,1,1,  0,0,0,0)) <= 0
    val b2 = Bra(s, NCRepr, QVector(-2,0,0,0,  0,0,0,0,  0,-1,0,1,  0,-1,0,-1)) <= 0
    val b3 = Bra(s, NCRepr, QVector(-2,0,0,0,  0,0,0,0,  0,1,1,0,  0,1,-1,0)) <= 0
    def i = extend(List(b1,b2,b3))
  }

  object Example2 {
    val s = Scenario(Vector(Vector(2,2,2,2)))
    val b1 = Bra(s, NCRepr, QVector(-3, 0, 2, 1, 0)) <= 0
    val b2 = Bra(s, NCRepr, QVector(-3, -1, 1, 0, -1)) <= 0
    val b3 = Bra(s, NCRepr, QVector(-3, -1, 1, 0, 1)) <= 0
    val b4 = Bra(s, NCRepr, QVector(-3, 0, 1, 2, 0)) <= 0
    def i = extend(List(b1,b2,b3,b4))
  }

  def extend(Bgen: List[Bra]) = {
    // TODO: use rational arithmetic fully, and be closer to original paper
    val s = Bgen.head.scenario
    assert(Bgen.forall(_.scenario == s))
    assert(s.parties.forall(party => party.inputs.forall(_ == 2)))
    val Bnsc = Bgen.map(_.as(NCRepr))
    val l = (1 /: Bnsc.map(b => abs(b.coeffs(0).toInt)))(lcm[Int])
    val B = Bnsc.map(b => Bra(s, NCRepr, {
      val ct = b.coeffs(0).toInt
      b.coeffs.mapElements((rat: Rational) => -rat.toInt*(l/ct)*signum(ct))
    }))

    val n = B.size
    val d = B.head.coeffs.length
    val coeff = alg.mutable.QVector.zeros(d*(n+1))
    for (l <- 1 to n) {
      val b = B(l-1).coeffs
      val c = alg.mutable.QVector.zeros(n+1)
      if (l == 1) {
        c(1) = -(n-3)
        for (k <- 2 to n)
          c(k) = 1
      } else {
        c(1) = 1
        c(l) = -1
      }
      for (i <- 1 until d; j <- 1 to n)
        coeff(i + d*j) += b(i)*c(j)
    }
    coeff(0) = -2*l
    Bra(Scenario(s.parties ++ Vector(Party(Vector.fill[Int](n)(2)))), NCRepr, coeff.toImmutable) <= 0
  }
}
