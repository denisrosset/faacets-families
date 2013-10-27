package com.faacets
package reader

import scala.util.parsing.combinator._
import alg._

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
        val s = Scenario(Vector(Vector.fill(mA)(2), Vector.fill(mB)(2)))
        (name, Bra(s, NGRepr, coeffs.toImmutable)) // <= 0
      }
    }
    def content = rep(inequality)
  }

  def readFile(filename: String) = {
    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines.mkString("\n")
    val ineqs = ItoFileParser.parse(ItoFileParser.content, lines).get
    source.close()
    ineqs.groupBy(_._1).mapValues(_.map(_._2))
  }
}
