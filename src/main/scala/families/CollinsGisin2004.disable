package com.faacets
package families

import spire.math.Rational
import data._

/* The Collins, Gisin inequalities given in
 * J. Phys. A: Math. Gen. 37 (2004) 1775–1787
 * for either binary inputs or binary outputs.
 */
object CollinsGisin2004 extends PaperInDatabase {
  val doi = Some("doi:10.1088/0305-4470/37/5/021")
  val arxiv = Some("arXiv:quant-ph/0306129")
  val paperKey = "CollinsGisin2004"

  def inequalities = for (m <- 2 to 5; n <- 2 to 5) yield {
    val b = apply(m, n)
    val facet = ((m, n)) match {
      case (2, _) if n <= 7 => Some(true)
      case (3, _) if n <= 6 => Some(true)
      case (_, 2) => Some(true) // Avis&Ito 2007
      case (4, _) if n <= 4 => Some(true)
      case (5, 3) => Some(true)
      case _ => None
    }
    ((s"I$m$m$n$n", Inequality(
      bra = b,
      localBound = Some(Rational.zero),
      localFacet = facet,
      description = s"Inequality I$m$m$n$n in CollinsGisin2004.")))
  }

  def matX(n: Int) = alg.immutable.QMatrix.tabulate(n-1,n-1)( (r,c) => if(r + c < n - 1) Rational.one else Rational.zero )
  def matY(n: Int) = alg.immutable.QMatrix.tabulate(n-1,n-1)( (r,c) => if(r + c >= n - 2) Rational.one else Rational.zero )
  def matZ(n: Int) = alg.immutable.QMatrix.tabulate(n-1,n-1)( (r,c) => if(r != n - 2 && r + c >= n - 2) Rational.one else Rational.zero )

  def apply(m: Int, n: Int) = {
    val mat = matrix(m, n)
    val coeffs = alg.immutable.QVector(mat.t.elements.toArray)
    val outputs = Vector.fill(m)(n)
    Bra(Scenario(Vector(Party(outputs), Party(outputs))), NGRepr, coeffs)
  }

  def matrix(m: Int, n: Int) = {
    val mat = alg.mutable.QMatrix.zeros(1 + m * (n - 1), 1 + m * (n - 1))
    // in the paper, Bob has the rows, Alice the columns
    val mX = matX(n)
    val mY = matY(n)
    val mZ = matZ(n)
    mat(0, 1 until n) = -1
    for (y <- 0 until m) {
      val rowStart = y * (n - 1) + 1
      val rowEnd = (y + 1) * (n - 1)
      mat(rowStart to rowEnd, 0) = -(m - 1) + y
      for (x <- 0 until m) {
        val colStart = x * (n - 1) + 1
        val colEnd = (x + 1) * (n - 1)
        val block = (x + y) match {
          case s if s < m - 1 => mX
          case s if s == m - 1 => mY
          case s if s == m => -mY
          case _ => -mZ
        }
        mat(rowStart to rowEnd, colStart to colEnd) = block
      }
    }
    mat
  }
  val ICHSH = Bra(Scenario("{[2 2] [2 2]}"), NGRepr, alg.immutable.QVector(
     0, -1,  0,
    -1,  1,  1,
     0,  1, -1)) // <= 0

  val I3322 = Bra(Scenario("{[2 2 2] [2 2 2]}"), NGRepr, alg.immutable.QVector(
     0, -1,  0,  0,
    -2,  1,  1,  1,
    -1,  1,  1, -1,
     0,  1, -1,  0)) // <= 0

  val I4422 = Bra(Scenario("{[2 2 2 2] [2 2 2 2]}"), NGRepr, alg.immutable.QVector(
     0, -1,  0,  0,  0,
    -3,  1,  1,  1,  1,
    -2,  1,  1,  1, -1,
    -1,  1,  1, -1,  0,
     0,  1, -1,  0,  0)) // <= 0

  val I2233 = Bra(Scenario("{[3 3] [3 3]}"), NGRepr, alg.immutable.QVector(
     0, -1, -1,  0,  0,
    -1,  1,  1,  0,  1,
    -1,  1,  0,  1,  1,
     0,  0,  1,  0, -1,
     0,  1,  1, -1, -1)) // <= 0

  val I2244 = Bra(Scenario("{[4 4] [4 4]}"), NGRepr, alg.immutable.QVector(
     0, -1, -1, -1,  0,  0,  0,
    -1,  1,  1,  1,  0,  0,  1,
    -1,  1,  1,  0,  0,  1,  1,
    -1,  1,  0,  0,  1,  1,  1,
     0,  0,  0,  1,  0,  0, -1,
     0,  0,  1,  1,  0, -1, -1,
     0,  1,  1,  1, -1, -1, -1)) // <= 0
}
