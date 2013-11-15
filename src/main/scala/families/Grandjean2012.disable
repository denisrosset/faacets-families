package com.faacets
package families

import alg._
import net.alasc.{ind2sub, sub2ind}
import data._
import spire.math.Rational

/** Some inequalities given in Grandjean et al. 2012
  */
object Grandjean2012 extends PaperInDatabase {
  val doi = Some("doi:10.1103/PhysRevA.85.052113")
  val arxiv = Some("arXiv:1204.3829")
  val paperKey = "Grandjean2012"

  def apply(k: Int) = eq1(k)

  def eq1(k: Int) = {
    val s = Scenario(Vector(Vector(k,k), Vector(k,k), Vector(k,k)))
    val dim = List(k,2,k,2,k,2)
    val P = alg.mutable.QVector.zeros(dim.product)
    def I(a:Int,x:Int,b:Int,y:Int,c:Int,z:Int) = sub2ind(dim, List(a,x,b,y,c,z))
    def brkt(n: Int) = ((4*k + n) % k)
    for (a <- 0 until k; b <- 0 until k; c <- 0 until k) {
      P(I(a,1,b,0,c,0)) += brkt(a - b + c)
      P(I(a,0,b,1,c,0)) += brkt(a + b - c)
      P(I(a,0,b,0,c,1)) += brkt(-a + b + c)
      P(I(a,1,b,1,c,1)) += brkt(-a - b - c - 1)
    }
    val bound = k - 1
    P *= 8
    P :-= bound
    Bra(s, FRepr, -P.toImmutable) // <= 0
  }

  def eq18(k: Int) = {
    val s = Scenario(Vector(Vector(k,k), Vector(k,k), Vector(k,k)))
    val dim = List(k,2,k,2,k,2)
    val P = alg.mutable.QVector.zeros(dim.product)
    def I(a:Int,x:Int,b:Int,y:Int,c:Int,z:Int) = sub2ind(dim, List(a,x,b,y,c,z))
    def brkt(n: Int) = ((4*k + n) % k)
    for (a <- 0 until k; b <- 0 until k; c <- 0 until k) {
      // first line
      P(I(a,1,b,1,c,1)) += brkt(a + b + c)
      P(I(a,1,b,1,c,1)) += brkt(a + b + c + 1)
      // second line and sym terms
      P(I(a,1,b,0,c,0)) += brkt(a + b + c)
      P(I(a,1,b,0,c,0)) += brkt(a + b + c + 1)
      P(I(a,0,b,1,c,0)) += brkt(a + b + c)
      P(I(a,0,b,1,c,0)) += brkt(a + b + c + 1)
      P(I(a,0,b,0,c,1)) += brkt(a + b + c)
      P(I(a,0,b,0,c,1)) += brkt(a + b + c + 1)
      // third line
      P(I(a,0,b,0,c,0)) -= 3*brkt(a + b + c)
      P(I(a,0,b,0,c,0)) -= 2*brkt(a + b + c + 1)
      // fourth line and sym terms
      P(I(a,1,b,1,c,0)) += brkt(a + b + c)
      P(I(a,1,b,0,c,1)) += brkt(a + b + c)
      P(I(a,0,b,1,c,1)) += brkt(a + b + c)
    }
    val bound = 2
    P *= 8
    P :-= bound
    Bra(s, FRepr, -P.toImmutable) // <= 0
  }

  def b1(k: Int) = {
    val s = Scenario(Vector(Vector(k,k), Vector(k,k), Vector(k,k)))
    val dim = List(k,2,k,2,k,2)
    val P = alg.mutable.QVector.zeros(dim.product)
    def I(a:Int,x:Int,b:Int,y:Int,c:Int,z:Int) = sub2ind(dim, List(a,x,b,y,c,z))
    def brkt(n: Int) = ((4*k + n) % k)
    for (a <- 0 until k; b <- 0 until k; c <- 0 until k) {
      // first line
      P(I(a,0,b,0,c,0)) += 2*brkt(a + b + c)
      P(I(a,0,b,0,c,0)) += 2*brkt(- a - b - c - 1)
      // second line
      P(I(a,0,b,0,c,0)) += brkt(- a - b - c)
      P(I(a,1,b,1,c,1)) += 3*brkt(- a - b - c - 1)
      // third line
      P(I(a,1,b,1,c,1)) += brkt(a + b + c - 1)
      P(I(a,1,b,1,c,1)) += brkt(a + b + c)
      // fourth line and sym terms
      P(I(a,1,b,0,c,0)) += brkt(-a + b + c)
      P(I(a,0,b,1,c,0)) += brkt(a - b + c)
      P(I(a,0,b,0,c,1)) += brkt(a + b - c)
      P(I(a,0,b,1,c,1)) += brkt(-a + b + c)
      P(I(a,1,b,0,c,1)) += brkt(a - b + c)
      P(I(a,1,b,1,c,0)) += brkt(a + b - c)
    }
    val bound = 6*(k - 1)
    P *= 8
    P :-= bound
    Bra(s, FRepr, -P.toImmutable) // <= 0
  }

  import scala.util.parsing.combinator._
  object EParser extends RegexParsers {
    def sign = ("+" ^^ (s => 1)) | ("-" ^^ (s => -1))
    def posinteger: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def integer: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
    def input: Parser[Int] = ("0" ^^ { x => 0 }) | ("1" ^^ { x => 1 })
    type EType = (Int, List[Int])
    def E: Parser[EType] = "E(" ~> (integer ~ (("|" ~> repN(3, input)) <~ ")")) ^^ {
      case (j ~ l) => (j, l)
    }
    def sym = "+" ~ "sym"
    type TermType = (Int, EType)
    def firstTerm: Parser[TermType] = opt(sign) ~ opt(posinteger) ~ E ^^ {
      case (Some(s) ~ Some(coeff) ~ e) => (s*coeff, e)
      case (None ~ Some(coeff) ~ e) => (coeff, e)
      case (Some(s) ~ None ~ e) => (s, e)
      case (None ~ None ~ e) => (1, e)
    }
    def nextTerm: Parser[TermType] = sign ~ opt(posinteger) ~ E ^^ {
      case (s ~ Some(coeff) ~ e) => (s*coeff, e)
      case (s ~ None ~ e) => (s, e)
    }
    def ineq = firstTerm ~ rep(nextTerm) ~ ((sym ~ "<=") ~> integer) ^^ {
      case (first ~ list ~ bound) => (first :: list, bound)
    }
  }

  def parse(str: String) = {
    val s = Scenario(Vector(Vector(3,3), Vector(3,3), Vector(3,3)))
    val dim = List(3,2,3,2,3,2)
    val P = alg.mutable.QVector.zeros(dim.product)
    def I(a:Int,x:Int,b:Int,y:Int,c:Int,z:Int) = sub2ind(dim, List(a,x,b,y,c,z))
    def cond(n: Int) = ((12 + n) % 3 == 0)
    val (terms, bound) = EParser.parse(EParser.ineq, str).get
    for (t <- terms) {
      val (coeff, (j, l)) = t
      for (a <- 0 until 3; b <- 0 until 3; c <- 0 until 3) {
        if (cond(a + b + c - j))
          for (x :: y :: z :: Nil <- l.permutations)
            P(sub2ind(dim, List(a,x,b,y,c,z))) += coeff
      }
    }
    if (bound != 0) {
      P *= 4
      P :-= bound
    }
    Bra(s, FRepr, P.toImmutable) // <= 0
  }
  def a = List(a1, a2, a3, a4, a5, a6, a7, a8, a9)
  def a1 = parse("E(2|000) - E(1|001) - E(1|011) - E(2|011) + 2E(2|111) + sym <= 0")
  def a2 = parse("-2E(1|000) - E(1|001) -2E(1|011) + 2E(1|111) + sym <= 0")
  def a3 = parse("-8E(1|000) - 2E(2|000) - E(1|001) + 2E(2|001) - 2E(1|011) - 2E(2|011) + 2E(1|111) - E(2|111) + sym <= 0")
  def a4 = parse("-2E(1|000) - E(2|000) - E(1|001) - 2E(2|001) - 2E(1|011) - E(2|011) + 5E(1|111) + 4E(2|111) + sym <= 0")
  def a5 = parse("-2E(1|000) - 2E(2|000) - E(1|001) - E(2|001) - 2E(1|011) - 2E(2|011) + 5E(1|111) + 5E(2|111) + sym <= 0")
  def a6 = parse("-E(1|000) - E(1|001) - E(2|001) - 3E(1|011) - E(2|011) + 4E(1|111) + 3E(2|111)  + sym <= 0")
  def a7 = parse("-3E(1|000) - E(2|000) - E(1|001) - E(2|001) - E(1|011) + 3E(1|111) + E(2|111)  + sym <= 0")
  def a8 = parse(" -6E(1|000) - 3E(2|000) - E(1|001) - 2E(2|001) - E(1|011) + E(2|011) + 3E(1|111)  + sym <= 0")
  def a9 = parse(" -3E(1|000) + E(2|000) - E(1|001) - 4E(1|011) - E(2|011) + 3E(1|111) + 2E(2|111)  + sym <= 0")

  def a2bis = {
    val s = Scenario(Vector(Vector(3,3), Vector(3,3), Vector(3,3)))
    val dim = List(3,2,3,2,3,2)
    val P = alg.mutable.QVector.zeros(dim.product)
    def I(a:Int,x:Int,b:Int,y:Int,c:Int,z:Int) = sub2ind(dim, List(a,x,b,y,c,z))
    def cond(n: Int) = ((12 + n) % 3 == 0)
    for (a <- 0 until 3; b <- 0 until 3; c <- 0 until 3) {
      if (cond(a+b+c-1)) {
        P(I(a,0,b,0,c,0)) -= 2
        P(I(a,1,b,0,c,0)) -= 1
        P(I(a,0,b,1,c,0)) -= 1
        P(I(a,0,b,0,c,1)) -= 1
        P(I(a,1,b,1,c,0)) -= 2
        P(I(a,1,b,0,c,1)) -= 2
        P(I(a,0,b,1,c,1)) -= 2
        P(I(a,1,b,1,c,1)) += 2
      }
    }
    Bra(s, FRepr, P.toImmutable) // <= 0
  }

  def inequalities = (for (k <- 2 to 3) yield ((s"Eq1_$k", Inequality(
    bra = eq1(k),
    localBound = Some(Rational.zero),
    localFacet = Some(eq1(k).isFacet),
    description = s"Eq. 1 for $k outputs in Grandjean2012.",
    findGroup = true)))) ++ 
  (for (k <- 2 to 3) yield ((s"Eq18_$k", Inequality(
    bra = eq18(k),
    localBound = Some(Rational.zero),
    localFacet = Some(eq18(k).isFacet),
    description = s"Eq. 18 for $k outputs in Grandjean2012.",
    findGroup = true)))) ++ 
  (for (k <- 2 to 3) yield ((s"B1_$k", Inequality(
        bra = b1(k),
        localBound = Some(Rational.zero),
        localFacet = Some(b1(k).isFacet),
        description = s"Eq. B1 for $k outputs in Grandjean2012.",
        findGroup = true)))) ++
  Seq(("A1", a1), ("A2", a2), ("A3", a3), ("A4", a4),
    ("A5", a5), ("A6", a6), ("A7", a7), ("A8", a8), ("A9", a9)).map {
    case (key, bra) => {
      ((key, Inequality(
        bra = bra,
        localBound = Some(Rational.zero),
        localFacet = Some(bra.isFacet),
        description = s"$key in Grandjean2012.",
        findGroup = true)))
    }
  }
}
