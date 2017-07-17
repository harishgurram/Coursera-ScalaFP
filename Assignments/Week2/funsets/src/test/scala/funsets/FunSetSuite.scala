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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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
    
    val s4 = (x:Int) => x>0 && x<=100 // set 1 to 100
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

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect contains only common elements of both sets") {
    new TestSets {
      val s1s2 = union(s1,s2) //{1} union {2} = {1,2}
      val s = intersect(s1, s1s2) // {1} intersect {1,2} = {1}
      assert(contains(s,1), "intersect contains 1")
      assert(!contains(s,2), "intersect not contains 2")
      assert(contains(intersect(s3,s4),3), "intersect contains 3")
    }
  }

  test("diff: contains only elements of set1 but not set2") {
    new TestSets {
      
      val s = diff(s4, union(union(s1,s2),s3)) // {1 to 100} diff union({1},{2},{3}) = {4 to 100}
      assert(contains(s,4), "diff not contains 1")
      assert(contains(s,6), "diff contains 6")
    }
  }
  
  test("Filter contains only elements of set1 filtered by f") {
    new TestSets {
      
      val s = filter(s4, x=> x>55) // {1 to 100} diff (x>55) = {56 to 100}
      assert(!contains(s,55), "diff not contains 55")
      assert(contains(s,66), "intersect contains 66")
    }
  }
  
  test("Forall: s4 holds f for all x or not") {
    new TestSets {
      assert(!forall(s4, x => x>55), "all s4 is not > 55")
      assert(forall(s4,x=>(x>=1&&x<=100)), "all s4 is 1 to 100")
    }
  }
  
  test("Exists: s4 contain at least 1 element that hols f") {
    new TestSets {
      assert(exists(s4, x => x>55), " s4 contain atleast one elemnt > 55")
      assert(!exists(s4,x=>x<1), " s4 does not contain elemnt is <1")
    }
  }
  
  test("map: transform {1,2} to {7,8}") {
    new TestSets {
      val s = map(union(s1,s2),x=>x+6)
      assert(contains(s, 8), " s contain 8")
      assert(!contains(s,1), " s does not contain 1")
    }
  }

}
