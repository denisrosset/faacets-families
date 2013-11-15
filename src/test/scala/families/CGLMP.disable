package com.faacets
package families

import org.scalatest.FunSuite
 
class CGLMPSuite extends FunSuite {
  test("I2233 is CGLMP(3)") {
    val I1 = CollinsGisin2004.I2233
    val I2 = CGLMP(3)
    assert(I1.canonicalForm == I2.canonicalForm)
  }

  test("The original formulation and the formulation of Acin, Gill, Gisin are equivalent for d=2,3,4") {
    for (d <- 2 to 4) {
      val I1 = CGLMP(d)
      val I2 = CGLMP.AGGForm(d)
      assert(I1.canonicalForm == I2.canonicalForm)
    }
  }
}
