package com.faacets
package families

import data._

/** Trait for families that are defined in a publication. */
trait PaperInDatabase {
  val doi: Option[String]
  val arxiv: Option[String]
  val paperKey: String
  def inequalities: Iterable[(String, Inequality)]

  def write {
    root.pubs.apply(paperKey).mkdirs
    for ((inequalityKey, ineq) <- inequalities) {
      for (source <- List(doi, arxiv).flatten)
        ineq.sources.add(source)
      ineq.path = Some(root.pubs.apply(paperKey).apply(inequalityKey))
      ineq.write()
    }
  }
}
