/*
# Readers for Bell inequality file formats
*/

package com.faacets
package reader

import scala.util.parsing.combinator._
import alg._
import spire.math.Rational

import impl._

trait Reader {
  def readFile(filename: String): Any // TODO: define interface for reader results
}

/*
## Bancal et al. file formats

See URL [http://www.gapoptic.unige.ch/Publications/bellinequalities].
*/

object BancalReader {
  object BancalLineParser extends RegexParsers {
    override val whiteSpace = """([ \t])+""".r
    def integer: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
    def line(d: Int) = repN(d, integer)
  }

  def readSymmetric522File(filename: String) = {
    val file = new java.io.File(filename)
    assert(file.exists == true)
    val party22 = Party(Vector(2, 2))
    val scenario = Scenario(Vector.fill(5)(party22))
    val source = scala.io.Source.fromFile(filename)
    def termsForNParties(n: Int): Seq[Set[Int]] = {
      (for (i <- n to 0 by -1) yield {
        val elements = List.tabulate(n)(j => {
          val a = 1
          val x = (if (j >= i) 2 else 1)
          party22.GOutputElement(a, x)
        }) ++ List.fill(5 - n)(party22.GMarginalElement)
        import net.alasc.{Sym, Dom}
        val permGroup = Sym(5)
        val representatives = permGroup.elements.map( p =>
          List.tabulate(5)(j => elements(p.image(Dom._0(j))._0))
        ).toList
        representatives.map(scenario.NGRepr.termIndexHelper(_)).toSet
      }).toList
    }
    val indices: Seq[Set[Int]] = Seq(Set(0)) ++ ((1 to 5) flatMap termsForNParties)
    val dsym = indices.size
    println(dsym)
    assert(dsym == 21)
    val d = scenario.NGRepr.size
    var index = 0
    val ineqs = for(line <- source.getLines().toArray) yield {
      if (index % 1000 == 0) println(index)
      index += 1
      val res = BancalLineParser.parse(BancalLineParser.line(dsym), line)
      if (res.successful) {
        val coeffs = Array.fill(d)(0)
        (res.get zip indices) foreach {
          case (coeff, indexSet) =>
            indexSet foreach { coeffs(_) = coeff }
        }
        Some(Bra(scenario.NGRepr, alg.immutable.QVector(coeffs))) // <= 0
      }
      else
        None
    }
    source.close
    ineqs.flatten
  }

  def readFile(filename: String, n: Int, m: Int, k: Int) = {
    val file = new java.io.File(filename)
    assert(file.exists == true)
    val scenario = Scenario(Vector.fill(n)(Party(Vector.fill(m)(k))))
    val d = scenario.NGRepr.size
    val source = scala.io.Source.fromFile(filename)
    val ineqs = for(line <- source.getLines().toArray) yield {
      val res = BancalLineParser.parse(BancalLineParser.line(d), line)
      if (res.successful)
        Some(Bra(scenario.NGRepr, alg.immutable.QVector(res.get.map(Rational(_)).toArray))) // <= 0
      else
        None
    }
    source.close
    ineqs.flatten
  }
}

/*
## Ito et al. file formats
*/

object ItoReader extends Reader {
  object ItoFileParser extends RegexParsers {
    override val whiteSpace = """([ \t])+""".r
    def crlf = "\r\n" | "\n"
    def integer: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
    def first_line = ("* (" ~> """[^)]*""".r) <~ ("""[^\r\n]*""".r ~ crlf)
    def inputs(t: String) = (t ~> integer ~ integer)
    def cutpolytopeinfo = (integer ~ integer ~ integer)
    def coeff_line(t: String) = (inputs(t) <~ ("|" ~ cutpolytopeinfo ~ "|")) >> (
      m => (repN(m._1+m._2+m._1*m._2, integer) <~ ("<=" ~ "0" ~ crlf) ^^ ( list =>
        (m._1, m._2, list) ) )
    )
    def inequality = first_line ~ (coeff_line("Cut") ~> coeff_line("Cor")) ^^ {
      case (name ~ tup) => {
        val (mA, mB, list) = tup
        val (c1, c2) = list.splitAt(mA + mB)
        val (ca1, cb1) = c1.splitAt(mA)
        val coeffs = alg.mutable.QVector.zeros((mA+1)*(mB+1))
        for (a <- 0 until mA)
          coeffs(a + 1) = ca1(a)
        for (b <- 0 until mB)
          coeffs((b + 1) * (mA + 1)) = cb1(b)
        for (a <- 0 until mA; b <- 0 until mB)
          coeffs((b + 1) * (mA + 1) + a + 1) = c2(a * mB + b)
        val s = Scenario(Vector(Vector.fill(mA)(2), Vector.fill(mB)(2)).map(Party(_)))
        (name, Bra(s.NGRepr, coeffs.toImmutable)) // <= 0
      }
    }
    def content = rep(inequality)
  }

  def readFile(filename: String) = {
    val source = scala.io.Source.fromFile(filename)
    val blocksOf3lines = source.getLines.grouped(3)
    val ineqs: Seq[(String, Bra)] = (blocksOf3lines.flatMap {
      case lines =>
        val theLines = lines.mkString("\n") + "\n"
        val result = ItoFileParser.parseAll(ItoFileParser.content, theLines)
        result match {
          case ItoFileParser.Success(i, _) => Some(i)
          case err: ItoFileParser.NoSuccess => { println(err.toString); None }

        }
    }).flatten.toList
    source.close()
    ineqs
  }
}
