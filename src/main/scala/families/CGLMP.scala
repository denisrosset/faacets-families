package com.faacets
package families

import alg._
import net.alasc.{ind2sub, sub2ind}
import data._
import spire.math.Rational

object AcinCGLMP2005 extends PaperInDatabase {
  val doi = Some("doi:10.1103/PhysRevLett.95.210402")
  val arxiv = Some("arXiv:quant-ph/0506225")
  val paperKey = "AcinCGLMP2005"

  def inequalities = for (d <- 2 to 6) yield (((s"$d"), Inequality(
    bra = apply(d),
    localBound = Some(Rational.zero),
    localFacet = Some(true),
    description = s"Form of the CGLMP inequality for dimension $d given by Acin in 2005.",
    findGroup = true)))

  /** Form of the CGLMP inequality given in Acin et al. in 2005.
    * 
    * The paper reference is
    * "Optimal Bell Tests Do Not Require Maximally Entangled States",
    * from A. Acin, R. Gill and N. Gisin, PRL 95, 210402 (2005)
    * e-print: arXiv:quant-ph/0506225
    * 
    * @param d   Dimension of the CGLMP inequality.
    * @return the CGLMP inequality constructed.
    */
  def apply(d: Int) = {
    val s = Scenario(Vector(Vector(d,d), Vector(d,d)))
    val dim = List(d,2,d,2)
    val beta = alg.mutable.QVector.zeros(dim.product)
    for (a <- 0 until d; b <- 0 until d) {
      def brkt(k: Int) = ((4*d + k) % d)
      beta(sub2ind(dim, List(a,0,b,0))) += brkt(a - b)
      beta(sub2ind(dim, List(a,1,b,0))) += brkt(b - a)
      beta(sub2ind(dim, List(a,1,b,1))) += brkt(a - b)
      beta(sub2ind(dim, List(a,0,b,1))) += brkt(b - a - 1)
    }
    beta *= 4
    beta :-= (d - 1)
    Bra(s, FRepr, beta.toImmutable) >= 0
  }
}

/** The Collins, Gisin, Linden, Massar and Popescu inequality for d dimensional systems.
  * 
  * The orginal inequality was described in
  * Phys. Rev. Lett. 88, 040404 (2002) / arXiv:quant-ph/0106024
  * and proven to be tight by Masanes in arXiv:quant-ph/0210073.
  */
object CGLMP2002 extends PaperInDatabase {
  val doi = Some("doi:10.1103/PhysRevLett.88.040404")
  val arxiv = Some("arXiv:quant-ph/0106024")
  val paperKey = "CGLMP2002"

  def inequalities = for (d <- 2 to 6) yield (((s"$d"), Inequality(
      bra = apply(d),
      localBound = Some(Rational.zero),
      localFacet = Some(true),
      sources = List("arXiv:quant-ph/0210073"),
      description = s"CGLMP inequality for dimension $d given by Collins et al. in 2001, proven tight by Masanes in 2002",
      findGroup = true)))

  /** Original form of the CGLMP inequality given by Collins et al. in 2001.
    * 
    * @param d   Dimension of the CGLMP inequality.
    * @return the CGLMP inequality constructed.
    */
  def apply(d: Int) = {
    val s = Scenario(Vector(Vector(d,d), Vector(d,d)))
    val krange = (0 to (d/2-1))
    val ineqbound = 2 * (d-1)
    // coefficients of inequality
    val dim = List(d,2,d,2)
    val beta = alg.mutable.QVector.zeros(dim.product)
    for (k <- krange) {
      val coeff = (d - 1) - 2 * k
      for (a <- 0 until d; b <- 0 until d) {
        // P(A1 - B1 = k) - P(A1 - B1 = - (k+1) )
        val ind11 = sub2ind(dim, List(a, 0, b, 0))
        val ind12 = sub2ind(dim, List(a, 0, b, 1))
        val ind21 = sub2ind(dim, List(a, 1, b, 0))
        val ind22 = sub2ind(dim, List(a, 1, b, 1))
        def cond(n: Int) = ((4*d + n) % d == 0)
        if (cond(a-b-k))
          beta(ind11) += coeff
        if (cond(b-a-k-1))
          beta(ind21) += coeff
        if (cond(a-b-k))
          beta(ind22) += coeff
        if (cond(b-a-k))
          beta(ind12) += coeff
        
        if (cond(a-b+k+1))
          beta(ind11) -= coeff
        if (cond(b-a+k))
          beta(ind21) -= coeff
        if (cond(a-b+k+1))
          beta(ind22) -= coeff
        if (cond(b-a+k+1))
          beta(ind12) -= coeff
      }
    }
    beta *= 4
    beta :-= ineqbound
    Bra(s, FRepr, beta.toImmutable) <= 0
  }
}
