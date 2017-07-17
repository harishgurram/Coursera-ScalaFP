package objsets

import TweetReader._


/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def union(that: TweetSet): TweetSet //implement traversal specific logic in sub classes
  
  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def mostRetweeted: Tweet //specific to implementation
  
    val count: Long
    
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def descendingByRetweet: TweetList //implementation specific for empty and non-empty sets
  
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc 
    //Nothing in the empty set to filter. just return the input acc set
  
    def union(that: TweetSet): TweetSet = that // just return other set
    
    def mostRetweeted: Tweet = throw new NoSuchElementException("Empty.mostRetweeted")
    
    val count: Long = 0
    
    def descendingByRetweet: TweetList = Nil //No Tweets to list
    
  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  
  
  
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
      val leftSet = left.filterAcc(p, acc) //filter left tree
      val rightSet = right.filterAcc(p, leftSet) // filter right tree with acc as filtered left tree
      if (p(elem)) rightSet.incl(elem) else rightSet
    }
    
    // add current elem to that, union that with right and union result with left sub sets
    def union(that: TweetSet): TweetSet = left union right union that incl elem
    
  
    val count: Long = 1+left.count+right.count
    
    //
    def mostRetweeted: Tweet = {
      
      // find the tweet with max retweets from left and Root
      val maxleftReTweeted = if (left.count > 0) { // check if sub tree has elements - prevents exceptions
        val leftMax = left.mostRetweeted
        if (leftMax.retweets > elem.retweets) leftMax else elem
      } else elem
      
      
      // find the tweet with max retweets in left+Root, Right tree
      if (right.count > 0) {
        val rightMax = right.mostRetweeted
        if (rightMax.retweets > maxleftReTweeted.retweets) rightMax else maxleftReTweeted
      }
      else maxleftReTweeted
    }
    
    def descendingByRetweet: TweetList ={
      
      //get most retweeted tweet in current set
      val mostRetweetedTweet = this.mostRetweeted
      
      //remove the most tweeted tweet from the set
      // call the descending order function recursively each time after removing current max tweeted tweet
      new Cons(mostRetweetedTweet, this.remove(mostRetweetedTweet).descendingByRetweet)
    }
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter {
    // pedicate: filter in only tweets with any of the google list keywords is present in text
    tweet => google.exists(keyword => tweet.text.contains(keyword)) }
  
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter {
    // pedicate: filter in only tweets with any of the apple list keywords is present in text
    tweet => apple.exists(keyword => tweet.text.contains(keyword)) }
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
     lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
  }

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
