package com.faacets
package families

import alg._
import net.alasc.{ind2sub, sub2ind}
import spire.math.Rational
import data._

object Acin2004 extends PaperInDatabase {
  val doi = Some("doi:10.1103/PhysRevLett.92.250404")
  val arxiv = None
  val paperKey = "Acin2004"

  def inequalities = Seq(("Eq4", Inequality(eq4,
    localBound = Some(Rational.zero),
    localFacet = Some(eq4.isFacet),
    description = "Coincidence Bell Inequality for three three-dimensional systems")))

  def eq4 = {
    val s = Scenario(Vector(Vector(3,3), Vector(3,3), Vector(3,3)))
    val ineqbound = 3
    // coefficients of inequality
    val dim = List(3,2,3,2,3,2)
    val P = alg.mutable.QVector.zeros(dim.product)
    def I(a:Int,x:Int,b:Int,y:Int,c:Int,z:Int) = sub2ind(dim, List(a,x,b,y,c,z))
    def cond(n: Int) = ((12 + n) % 3 == 0)
    for (a <- 0 until 3; b <- 0 until 3; c <- 0 until 3) {
      if (cond(a+b+c)) {
        P(I(a,0,b,0,c,0)) += 1
        P(I(a,1,b,1,c,1)) += 2
      }
      if (cond(a+b+c-1)) {
        P(I(a,0,b,1,c,1)) += 1
        P(I(a,1,b,0,c,1)) += 1
        P(I(a,1,b,1,c,0)) += 1
      }
      if (cond(a+b+c-2)) {
        P(I(a,1,b,0,c,0)) -= 1
        P(I(a,0,b,1,c,0)) -= 1
        P(I(a,0,b,0,c,1)) -= 1
      }
    }
    P *= 8
    P :-= ineqbound
    Bra(s, FRepr, P.toImmutable) // <= 0
  }
}
