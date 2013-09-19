package com.faacets
package families

import data._
import spire.math.Rational

object Sliwa2003 extends PaperInDatabase {
  val doi = Some("doi:10.1016/S0375-9601(03)01115-0")
  val arxiv = Some("arXiv:quant-ph/0305190")
  val paperKey = "Sliwa2003"

  val s = Scenario("{[2 2] [2 2] [2 2]}")
  val exprs = List(
    "1 <> -<A1>-<B1>+<A1B1>-<C1>+<A1C1>+<B1C1>-<A1B1C1>",
    "2 <> - <A1B1C1> - <A2B2C1> - <A2B1C2> + <A1B2C2>",
    "2 <> - <A1B1C1> - <A2B1C1> - <A1B2C2> + <A2B2C2>",
    "2 <> - 2<A1> - <B1C1> + <A1B1C1> - <B2C1> + <A1B2C1> - <B1C2> + <A1B1C2> + <B2C2> - <A1B2C2>",
    "3 <> - <A1> - <B1> - <A2B1> - <A1B2> + <A2B2> - <C1> - <A2C1> + <A1B1C1> + <A2B1C1> - <B2C1> + <A1B2C1> - <A1C2> + <A2C2> - <B1C2> + <A1B1C2> + <B2C2> - <A2B2C2>",
    "3 <> - <A1> - <B1> - <A1B1> - <C1> - <A2C1> + <A1B1C1> + <A2B1C1> - <B2C1> + <A1B2C1> - <A1C2> + <A2C2> + <B1C2> - <A2B1C2> - <B2C2> + <A1B2C2>",
    "4 <> - 3<A1B1C1> - <A2B1C1> - <A1B2C1> + <A2B2C1> - <A1B1C2> + <A2B1C2> + <A1B2C2> - <A2B2C2>",
    "4 <> - <A1B1> - <A2B1> - <A1B2> - <A2B2> - 2<A1B1C1> + 2<A2B2C1> - <A1B1C2> + <A2B1C2> + <A1B2C2> - <A2B2C2>",
    "4 <> - <A1B1> - <A2B1> - <A1B2> - <A2B2> - 2<A1B1C1> + 2<A1B2C1> - <A1B1C2> + <A2B1C2> - <A1B2C2> + <A2B2C2>",
    "4 <> - <A1B1> - <A2B1> - <A1B2> - <A2B2> - <A1C1> + <A2C1> - <B1C1> - <A1B1C1> + <B2C1> + <A2B2C1> - <A1C2> + <A2C2> + <B1C2> - <A2B1C2> - <B2C2> + <A1B2C2>",
    "4 <> - 2<A1B1> - 2<A2B2> - <A1B1C1> - <A2B1C1> + <A1B2C1> + <A2B2C1> - <A1B1C2> + <A2B1C2> - <A1B2C2> + <A2B2C2>",
    "4 <> - 2<A1B1> - 2<A2B2> - <A1C1> - <A2C1> + <B1C1> - <A2B1C1> + <B2C1> + <A1B2C1> - <A1C2> - <A2C2> + <B1C2> + <A2B1C2> + <B2C2> - <A1B2C2>",
    "4 <> - 2<A1B1> - 2<A2B1> - <A1B1C1> + <A2B1C1> - <A1B2C1> + <A2B2C1> - <A1B1C2> + <A2B1C2> + <A1B2C2> - <A2B2C2>",
    "4 <> - 2<A1B1> - 2<A2B1> - <A1C1> + <A2C1> - <A1B2C1> + <A2B2C1> - <A1C2> + <A2C2> + <A1B2C2> - <A2B2C2>",
    "4 <> - 2<A1B1>-2<A2B1>-<A1C1>-<A2C1>+2<B1C1>-<A1B2C1>+ <A2B2C1> - <A1C2> - <A2C2> + 2<B1C2> + <A1B2C2> - <A2B2C2>",
    "4 <> - <A1> - <A2> - <A1B1> - <A2B1> - <A1C1> - <A2C1> + 2<A2B1C1> - <A1B2C1> + <A2B2C1> - <A1B1C2> + <A2B1C2> + <A1B2C2> - <A2B2C2>",
    "4 <> - <A1> - <A2> - <A1B1> - <A2B1> - <A1C1> - <A2C1> + <A1B1C1> + <A2B1C1> - 2<A1B2C2> + 2<A2B2C2>",
    "4 <> - <A1> - <A2> - <A1B1> - <A2B1> - <A1C1> - <A2C1> + 2<B1C1> - <A1B2C1> + <A2B2C1> - <A1B1C2> + <A2B1C2> - 2<B2C2> + <A1B2C2> + <A2B2C2>",
    "4 <> - <A1> - <A2> - <A1B1> - <A2B1> - <A1C1> - <A2C1> + 2<B1C1> - 2<B2C1> + <A1B2C1> + <A2B2C1> - <A1B1C2> + <A2B1C2> - <A1B2C2> + <A2B2C2>",
    "4 <> - <A1> - <A2> - <A1B1> + <A2B1> - <A1B2> + <A2B2> - <A1C1> + <A2C1> + <B1C1> - <A1B1C1> - <A2B1C1> + <B2C1> - <A1B2C1> - <A2B2C1> - <B1C2> + <A1B1C2> + <A2B1C2> + <B2C2> - <A1B2C2> - <A2B2C2>",
    "4 <> - <A1> - <A2> - <B1> - <A1B1> - <B2> + <A2B2> - <A1C1> - <A2C1> - <B1C1> + 2<A1B1C1> + <A2B1C1> - <B2C1> + <A1B2C1> - <A1B1C2> + <A2B1C2> + <A1B2C2> - <A2B2C2>",
    "4 <> - <A1>-<A2>-<B1>-<A1B1>-<B2>+<A2B2>-<C1>- <A1C1> - <B1C1> + 2<A1B1C1> + <A2B1C1> + <A1B2C1> - <A2B2C1> - <C2> + <A2C2> + <A1B1C2> - <A2B1C2> + <B2C2> - <A1B2C2>",
    "4 <> - <A1> - <A2> - <B1> + <A1B1> + <A2B1> - <B2> + <A1B2> + <A2B2> - <A1C1> + <A2C1> + <A1B1C1> - <A2B1C1> + <A1B2C1> - <A2B2C1> - <B1C2> + <A1B1C2> + <A2B1C2> + <B2C2> - <A1B2C2> - <A2B2C2>",
    "5 <> - <A1> - <B1> - <A2B1> - <A1B2> - <A2B2> - <C1> - <A2C1> + <B1C1> - 2<A1B1C1> + <A2B1C1> + 2<A2B2C1> - <A1C2> - <A2C2> + 2<A2B1C2> + <A1B2C2> - <A2B2C2>",
    "5 <> - <A1> - <B1> - <A2B1> - <A1B2> - <A2B2> - <C1> - <A2C1> + <B1C1> - 2<A1B1C1> + <A2B1C1> + 2<A2B2C1> - <A1C2> - <A2C2> + 2<A1B1C2> - <A1B2C2> + <A2B2C2>",
    "5 <> - <A1> - <B1> - <A1B1> - 2<A2B2> - <C1> - <A1C1> - <B1C1> + <A1B1C1> + 2<A2B2C1> - 2<A2C2> + 2<A2B1C2> + 2<B2C2> - 2<A1B2C2>",
    "5 <> - 2<A1>-<A2>-<B1>+<A1B1>-<A1B2>-<A2B2>-<C1>+ <A1C1>-2<A1B1C1>+2<A2B1C1>-<B2C1>+<A1B2C1>-<A1C2>- <A2C2> - <B1C2> + <A1B1C2> - <B2C2> + 2<A1B2C2> + <A2B2C2>",
    "6 <> -<A1>-<A2>-<A1B1>+<A2B1>-<A1C1>+<A2C1>+<B1C1>- 2<A1B1C1> - <A2B1C1> - <B2C1> + <A1B2C1> + 2<A2B2C1> - <B1C2> + <A1B1C2> + 2<A2B1C2> - <B2C2> + 3<A1B2C2>",
    "6 <>-<A1>-<A2>-<A1B1>+<A2B1>-<A1C1>+<A2C1>+<B1C1>- 2<A1B1C1> - <A2B1C1> - <B2C1> + <A1B2C1> + 2<A2B2C1> - <B1C2> + 3<A1B1C2> - <B2C2> + <A1B2C2> + 2<A2B2C2>",
    "6 <> - <A1> - <A2> - 2<A1B1> + 2<A2B1> - <A1B2> + <A2B2> - <A1C1> + <A2C1> + <B1C1> - 2<A1B1C1> - <A2B1C1> + <B2C1> - <A1B2C1> - 2<A2B2C1> - <B1C2> + 2<A1B1C2> + <A2B1C2> + <B2C2> - 2<A1B2C2> - <A2B2C2>",
    "6 <>-<A1>-<A2>-<B1>+<A2B1>-<B2>+<A1B2>-<A1C1>+ <A2C1> - 2<A2B1C1> + <A1B2C1> - 3<A2B2C1> - <B1C2> + 2<A1B1C2> + <A2B1C2> + <B2C2> - 2<A1B2C2> - <A2B2C2>",
    "6 <>-<A1>-<A2>-<B1>+<A2B1>-<B2>+<A1B2>-2<A1C1>+ 2<A2C1>-2<A2B1C1>-2<A2B2C1>-<A1C2>+<A2C2>+<B1C2>- 2<A1B1C2> - <A2B1C2> - <B2C2> + <A1B2C2> + 2<A2B2C2>", 
    "6 <>-<A1>-<A2>-<B1>+<A2B1>-<B2>+<A1B2>-<C1>+ <A2C1> - 2<A2B1C1> + <B2C1> - 2<A1B2C1> - <A2B2C1> - <C2> + <A1C2> + <B1C2> - 2<A1B1C2> - <A2B1C2> - <A1B2C2> + 3<A2B2C2>",
    "6 <>-<A1>-<A2>-<B1>+<A2B1>-<B2>+<A1B2>-<C1>+ <A2C1> + <B1C1> + 2<A1B1C1> - <A2B1C1> + 2<B2C1> - 2<A1B2C1> - 2<A2B2C1> - <C2> + <A1C2> + 2<B1C2> + <B2C2> - <A1B2C2> + 2<A2B2C2>",
    "6 <> - <A1> - <A2> - <B1> + <A1B1> + 2<A2B1> - <B2> + 2<A1B2> + <A2B2> - <A1C1> + <A2C1> + <A1B1C1> - <A2B1C1> + 2<A1B2C1> - 2<A2B2C1> - <B1C2> + 2<A1B1C2> + <A2B1C2> + <B2C2> - 2<A1B2C2> - <A2B2C2>",
    "6 <> - 2<A1> - <A1B1> - <A2B1> - <A1B2> - <A2B2> - <A1C1> - <A2C1> - <B1C1> + 2<A1B1C1> - <A2B1C1> + <B2C1> - <A1B2C1> + 2<A2B2C1> - <A1C2> - <A2C2> + <B1C2> - <A1B1C2> + 2<A2B1C2> + <B2C2> - 2<A1B2C2> + <A2B2C2>",
    "6 <> - 2<A1> - <A1B1> - <A2B1> - <A1B2> - <A2B2> - <A1C1> - <A2C1> - <B1C1> + 3<A1B1C1> + <B2C1> - 2<A1B2C1> + <A2B2C1> - <A1C2> - <A2C2> + <B1C2> - 2<A1B1C2> + <A2B1C2> + <B2C2> - <A1B2C2> + 2<A2B2C2>",
    "6 <> - 2<A1> - 2<A1B1> - 2<A2B1> - <A1C1> - <A2C1> + <B1C1> - <A1B1C1> + 2<A2B1C1> - <B2C1> + 2<A1B2C1> - <A2B2C1> - <A1C2> - <A2C2> + <B1C2> - <A1B1C2> + 2<A2B1C2> + <B2C2> - 2<A1B2C2> + <A2B2C2>",
    "6 <> - 2<A1> - 2<B1> + <A1B1> - <A2B1> - <A1B2> - <A2B2> - 2<C1> + <A1C1> - <A2C1> + <B1C1> - 2<A1B1C1> + <A2B1C1> - <B2C1> + <A1B2C1> + 2<A2B2C1> - <A1C2> - <A2C2> - <B1C2> + <A1B1C2> + 2<A2B1C2> - <B2C2> + 2<A1B2C2> - <A2B2C2>",
    "6 <> - 2<A1> - 2<A2> - 2<B1> + <A1B1> + <A2B1> - <A1B2> - <A2B2> - <A1C1> - <A2C1> - 2<B1C1> + <A1B1C1> + <A2B1C1> - 2<B2C1> + 2<A1B2C1> + 2<A2B2C1> - <A1C2> + <A2C2> + 2<A1B1C2> - 2<A2B1C2> - <A1B2C2> + <A2B2C2>",
    "7 <> - <A1> - <B1> - <A1B1> - <C1> - <A2C1> + 3<A1B1C1> + <A2B1C1> - <B2C1> + <A1B2C1> + 2<A2B2C1> - <A1C2> + <A2C2> - <B1C2> + 4<A1B1C2> - <A2B1C2> + <B2C2> - <A1B2C2> - 2<A2B2C2>",
    "8 <> - <A1> - <A2> - <B1> - <A1B1> - <B2> + <A2B2> - <A1C1> + <A2C1> - <B1C1> + 2<A1B1C1> + <A2B1C1> + <B2C1> + <A1B2C1> - 4<A2B2C1> - 2<A2C2> + <A1B1C2> + 3<A2B1C2> - 2<B2C2> + 3<A1B2C2> + <A2B2C2>",
    "8 <> - 2<A1> - 2<B1> + <A1B1> - <A2B1> - <A1B2> + <A2B2> - <A1C1> - <A2C1> - <B1C1> + 2<A1B1C1> + 3<A2B1C1> + <B2C1> - <A1B2C1> - 2<A2B2C1> - <A1C2> + <A2C2> - <B1C2> + 3<A1B1C2> - <B2C2> + 4<A1B2C2> - <A2B2C2>",
    "8 <> - 2<A1> - 2<A2> - 2<A1B1> + 2<A2B1> - <A1C1> + <A2C1> + 2<B1C1> - 2<A1B1C1> - 2<A2B1C1> - 2<B2C1> + <A1B2C1> + 3<A2B2C1> - <A1C2> + <A2C2> + 2<B1C2> - 2<A1B1C2> - 2<A2B1C2> + 2<B2C2> - 3<A1B2C2> - <A2B2C2>",
    "8 <> - 3<A1> - <A2> - 2<A1B1> + 2<A2B1> - <A1B2> + <A2B2> - 2<A1C1> + 2<A2C1> + 2<B1C1> - 2<A1B1C1> - 2<A2B1C1> + 2<B2C1>-2<A1B2C1>-2<A2B2C1>-<A1C2>+<A2C2>+2<B1C2>- 2<A1B1C2> - 2<A2B1C2> - 2<B2C2> + 3<A1B2C2> + <A2B2C2>",
    "10 <> - 3<A1> - <A2> - 3<B1> + 2<A1B1> + <A2B1> - <B2> + <A1B2> + 2<A2B2> - 2<A1C1> + 2<A2C1> - <B1C1> + 3<A1B1C1> - 4<A2B1C1> - <B2C1> + <A1B2C1> - 2<A2B2C1> - <A1C2> - <A2C2> - 2<B1C2> + 3<A1B1C2> + <A2B1C2> + 2<B2C2> - 4<A1B2C2> - 2<A2B2C2>")

  val families = exprs.map( expr => (VecParser.parse(VecParser.bra(s), expr).get >= 0 ) )
  def inequalities = for (i <- 0 until 46) yield ((s"${i+1}", Inequality(
      bra = families(i),
      localBound = Some(Rational.zero),
      localFacet = Some(true),
      description = s"Family ${i+1} in Sliwa2003.")))
}
