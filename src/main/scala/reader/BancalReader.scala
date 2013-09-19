package com.faacets
package reader

import scala.util.parsing.combinator._
import alg._
import spire.math.Rational

/** Reads the symmetric inequalities provided by Jean-Daniel Bancal et al.
  * 
  * @note URL of the corresponding data files:
  *       http://www.gapoptic.unige.ch/Publications/bellinequalities
  */
object BancalReader {
  object BancalLineParser extends RegexParsers {
    override val whiteSpace = """([ \t])+""".r
    def integer: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
    def line(d: Int) = repN(d, integer)
  }

  def readFile(filename: String, n: Int, m: Int, k: Int) = {
    val file = new java.io.File(filename)
    assert(file.exists == true)
    val scenario = Scenario(Vector.fill(n)(Vector.fill(m)(k)))
    val d = NGRepr.sizeForScenario(scenario)
    val source = scala.io.Source.fromFile(filename)
    val ineqs = for(line <- source.getLines().toArray) yield {
      val res = BancalLineParser.parse(BancalLineParser.line(d), line)
      if (res.successful)
        Some(Bra(scenario, NGRepr, alg.immutable.QVector(res.get.map(Rational(_)).toArray)) <= 0)
      else
        None
    }
    source.close
    ineqs.flatten
  }
}
