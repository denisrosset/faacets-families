package com.faacets
package families

import alg._
import data._
import spire.math.Rational

object Mermin1990 extends PaperInDatabase {
  val doi = Some("doi:10.1103/PhysRevLett.65.1838")
  val arxiv = None
  val paperKey = "Mermin1990"
 
  def apply(n: Int) = {
    val dim = BigInt(3).pow(n).toInt
    val coeffs = alg.mutable.QVector.zeros(dim)
    def indexPhase(binaryIndex: Int, k: Int): (Int, Int) = {
      if (k == n)
        return ((0, 0))
      val bit = binaryIndex % 2
      val (coeffIndex, nextPhase) = indexPhase(binaryIndex / 2, k + 1)
      (((3 * coeffIndex + bit + 1), nextPhase + bit))
    }
    for (i <- 0 until (1 << n)) {
      val (coeffIndex, phase) = indexPhase(i, 0)
      (coeffIndex % 4) match {
        case 1 => coeffs(coeffIndex) += 1 // coeff is i
        case 3 => coeffs(coeffIndex) -= 1 // coeff is -i
        case _ => { }
      }
    }
    (n % 2) match {
      case 0 => coeffs(0) -= (1 << (n/2))
      case 1 => coeffs(0) -= (1 << ((n-1)/2))
    }
    val parties = Vector.fill(n)(Party(Vector(2,2)))
    Bra(Scenario(parties), NCRepr, coeffs.toImmutable)
  }

  def inequalities = for (n <- 3 to 4) yield ((s"eq11_$n", Inequality(
    bra = apply(n),
    localBound = Some(Rational.zero),
    localFacet = Some(n % 2 == 1),
    description = s"Mermin inequality for $n parties",
    findGroup = true)))
}
