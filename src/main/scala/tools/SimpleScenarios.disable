package com.faacets
package tools

import data._
import spire.math.Rational

object SimpleScenarios {
  val list = List(
    Scenario("{[2 2] [2 2]}"),
    Scenario("{[2 2 2] [2 2 2]}"),
    Scenario("{[3 2] [2 2]}"),
    Scenario("{[3 3] [2 2]}"),
    Scenario("{[3 3] [3 2]}"),
    Scenario("{[3 3] [3 3]}"),
    Scenario("{[2 2] [2 2] [2 2]}"),
    Scenario("{[2 2 2] [2 2]}"),
    Scenario("{[3 2 2] [2 2]}"),
    Scenario("{[3 3 2] [2 2]}"),
    Scenario("{[3 3 3] [2 2]}"),
    Scenario("{[2 2 2] [3 2]}"),
    Scenario("{[3 2 2] [3 2]}"),
    Scenario("{[3 3 2] [3 2]}"),
    Scenario("{[2 2 2] [3 3]}"),
    Scenario("{[3 2 2] [3 3]}")
  )/*
  def writeNS {
    root.mkdir("solved")
    val folder = root.solved
    for (s <- list) {
      val ident = "NS" + s.toIdentifier
      println(ident)
      folder.mkdir(ident)
      val newFolder = folder(ident)
      val boxes = NonSignalingPolytope(s).boxes
      for (b <- boxes) {
        val data = BoxData.fromNonSignalingBox(b)
        data.sources.add("Polytope solved by Sympol.")
        data.path = newFolder.nextKey.get
        data.write()
      }
    }
  }*/
  def writeLocal {
    root.solved.mkdirs
    for (s <- list) {
      val ident = "L" + s.toIdentifier
      val folder = root.solved.apply(ident)
      folder.mkdirs
      val ineqs = LocalPolytope(s).inequalities
      for (i <- ineqs) {
        val data = Inequality(
          bra = i,
          localBound = Some(Rational.zero),
          localFacet = Some(true),
          sources = List("Polytope solved by Sympol."),
          findGroup = true
        )
        data.path = Some(folder.makeNextInteger())
        data.write()
      }
    }
  }
  def write {
    writeLocal
    //writeNS
  }
}
