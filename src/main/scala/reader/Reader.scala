package com.faacets
package reader

trait Reader {
  def readFile(filename: String): Any // TODO: define interface for reader results
}
