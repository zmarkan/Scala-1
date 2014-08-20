package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  test("countChange: when money is 0 always return 1 posibilities"){
    assert(countChange(0, List(500,5,50,100,20,200,10)) === 1)
  }

  test("countChange: when money less than 0 always return 0"){
    assert(countChange(-1, List(500,5,50,100,20,200,10)) === 0)
  }

  test("countChange: when money is there but no coins return 0"){
    assert(countChange(1, List()) === 0)
  }
}
