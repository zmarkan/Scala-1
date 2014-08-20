package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s11 = singletonSet(11)
    val allElements = union(union(union(s1, s2), s3), s11)

    def isEven(element: Int) = element % 2 == 0
    def greaterThan10(element: Int) = element > 10
    def isPositiveInt(element: Int) = element > 0

  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect only contains elements in both sets"){
    new TestSets{
      val intersect1 = intersect(s1, s2)
      val intersect2 = intersect(union(s1, s2), s1)
      val intersect3 = intersect( union(s1, s2), s2)


      assert(!contains(intersect1, 1))
      assert(contains(intersect2, 1))
      assert(contains(intersect3, 2))
    }
  }

  test("diff contains only items in one set but not in other"){
    new TestSets{
      val diff1 = diff(s1, s2)
      val diff2 = diff(union(s1, s2), s3)
      val diff3 = diff(union(s1, s2), s1)

      assert(contains(diff1, 1))
      assert(contains(diff1, 2))
      assert(contains(diff2, 3))
      assert(contains(diff2, 3))
      assert(!contains(diff3, 1))
    }
  }

  test("filter returns subset of s for which p is true"){
    new TestSets{

      val filteredSet1 = filter(union(s1, s11), greaterThan10)
      val filteredSet2 = filter(union(union(union(s1, s2), s3), s11), isEven)

      assert(contains(filteredSet1, 11))
      assert(!contains(filteredSet1, 1))
      assert(contains(filteredSet2, 2))
      assert(!contains(filteredSet2, 3))
    }
  }
  
  test("forAll goes through the set and returns true if all elements satisfy the predicate"){
    new TestSets{


      assert(forall(allElements, isPositiveInt))
      assert(!forall(allElements, isEven))
    }
  }

  test("exists uses forall to check if an element exists that satisfies condition"){
    new TestSets{

      val existsPositiveInt = exists(union(allElements, singletonSet(-1)), isPositiveInt)
      val existsGreaterThan10 = exists(union(s1, s2), greaterThan10)

      assert(existsPositiveInt, "positive int should exist")
      assert(!existsGreaterThan10, "greater than 10 shouldn't exists")
    }
  }

  test("map iterates through set and applies function returning a transformed set"){
    new TestSets{

      def double(element: Int) = element * 2
      val doubledSet = map(allElements, double)

      assert(!contains(doubledSet, 1))
      assert(contains(doubledSet, 2))
      assert(contains(doubledSet, 22))
    }

  }
}
