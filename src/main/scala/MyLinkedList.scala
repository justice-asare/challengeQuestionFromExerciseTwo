import scala.annotation.tailrec

/**
 * Scala has a built in LinkedList under the List class, the aim here is to build our own (simplified) version
 * of a Linked List called MyLinkedList
 *
 * Exercises:
 *   1) Define a trait (MyLinkedList) and appropriate case classes and case objects for our linkedList to represent empty and non-empty
 *   2) Create a method head that returns the first element of our List
 *   3) Create a method tail that returns the remaining elements of our List
 *   4) Create a method isEmpty that returns whether our List is empty or not
 *   5) Create a method to add an element to a list, call this method :: as is standard in the Scala List Library
 *   6) Create a toString method that will return a string representation of the List with commas between each element e.g. [1,2,3,4,5]
 *   7) Create the appropriate method so that a List can be created with elements easily, i.e. val list = MyLinkedList(1,2,3,4,5)
 *
 * Notes: The List must be immutable.
 *
 */

// All lists are built from two fundamental building blocks, Nil and ::(Pronounced "cons")
// Nil represents the empty list
// The infix operator :: expresses list extension at the front. That is x :: xs represents a list whose
// first element is x, followed by (the elements of) list xs.

sealed trait MyLinkedList[+T] {
  def head: T
  def tail: MyLinkedList[T]
  def isEmpty: Boolean
  def ::[S >: T](element :S): MyLinkedList[S] // Add method
  def printElements: String
  override def toString: String = "[" + printElements + "]"
  def map[B](f:T => B): MyLinkedList[B]
  def filter(f: T => Boolean): MyLinkedList[T]
  def ++[S >: T](list: MyLinkedList[S]): MyLinkedList[S]
  def flatMap[B](f: T => MyLinkedList[B]): MyLinkedList[B]
  def forEach(f: T => Unit): Unit
  def length: Int
  def reverse: MyLinkedList[T]
  def fold[S >: T](start: S)(operator: (S, S) => S): S
  def reduce[S >: T](operator: (S, S) => S): S
}

// Empty
case object Empty extends MyLinkedList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException("Head of empty MyLinkedList")
  override def tail: MyLinkedList[Nothing] = throw new NoSuchElementException("Attempt to access tail of empty MyLinkedList")
  override def isEmpty: Boolean = true
  override def ::[S >: Nothing](element: S): MyLinkedList[S] = Cons(element, Empty)
  override def printElements: String = ""
  override def map[B](f: Nothing => B): MyLinkedList[B] = Empty
  override def filter(f: Nothing => Boolean): MyLinkedList[Nothing] = Empty
  override def ++[S >: Nothing](list: MyLinkedList[S]): MyLinkedList[S] = list
  override def flatMap[B](f: Nothing => MyLinkedList[B]): MyLinkedList[B] = Empty
  override def forEach(f: Nothing => Unit): Unit = ()
  override def reverse: MyLinkedList[Nothing] = Empty
  override def length: Int = 0
  override def fold[S >: Nothing](start: S)(operator: (S, S) => S): S = ???

  override def reduce[S >: Nothing](operator: (S, S) => S): S = ???
}

// Cons
case class Cons[T](h: T, t: MyLinkedList[T]) extends MyLinkedList[T] {
  override def head: T = h
  override def tail: MyLinkedList[T] = t
  override def isEmpty: Boolean = false
  override def ::[S >: T](element: S): MyLinkedList[S] = Cons(element, this)
  override def printElements: String =
    if(t.isEmpty) head.toString
    else head.toString + ", " + tail.printElements

  //B represents the type of List in which the map is defined and B is the type of the new List
  // To define a function argument, we have to use the => sign, f is a function that takes a single parameter
  // of type T and returns a result of type B
  override def map[B](f: T => B): MyLinkedList[B] = f(head) :: tail.map(f)
  override def filter(f: T => Boolean): MyLinkedList[T] =
    if(f(head)) head :: tail.filter(f)
    else tail.filter(f)
  override def ++[S >: T](list: MyLinkedList[S]): MyLinkedList[S] = Cons(h, t ++ list)
  override def flatMap[B](f: T => MyLinkedList[B]): MyLinkedList[B] = f(head) ++ tail.flatMap(f)
  override def forEach(f: T => Unit): Unit = {
    f(head)
    tail.forEach(f)
  }
  override def length: Int = 1 + tail.length
  override def reverse: MyLinkedList[T] = {
    @tailrec
    def reverseAcc(currList: MyLinkedList[T], reversedList: MyLinkedList[T]): MyLinkedList[T] = {
      if(currList.isEmpty) reversedList
      else reverseAcc(currList.tail, currList.head :: reversedList)
    }
    reverseAcc(this, Empty)
  }

  override def fold[S >: T](start: S)(operator: (S, S) => S): S =
    if(isEmpty) start
    else tail.fold(operator(start, head))(operator)

  /*
    List(1,2,3,4,5).fold(0)(_ + _) // 0 - start == List(1,2,3,4,5).reduce
    List(2,3,4,5).fold(1)(_ + _) // 0 + 1
    List(3,4,5).fold(3)(_ + _) // 0 + 1
    List(4,5).fold(6)(_ + _) // 0 + 1
    List(5).fold(10)(_ + _) // 0 + 1
    List().fold(15)(_ + _)
    15

   */

  override def reduce[S >: T](operator: (S, S) => S): S =
    if(length < 2) head
    else tail.tail.fold(operator(head, tail.head))(operator)
}


object MyLinkedList {

  def apply[T](items: T*): MyLinkedList[T] =
    if (items.isEmpty) Empty
    else Cons(items.head, apply(items.tail: _*))

  def main(args: Array[String]): Unit = {
    val myList: MyLinkedList[Int] = Cons(1, Cons(2, Cons (3, Empty)))
    val myList2: MyLinkedList[Int] = 1 :: 2 :: 3 :: 4 :: Empty

    val calvinsList = MyLinkedList(1,2,3,4,5,6,7,8,9,10)
    val calvinsList2 = MyLinkedList()
    val ivanList = MyLinkedList(80.5,78.3,23.5)
    val groceryList = MyLinkedList("Fish", "Yam", "Oil")
    val mapCalvinlist = calvinsList.map(_ + 1)
    val filterCalvinList = calvinsList.filter(_ < 3)

    val myEmptyList = Empty
//    println(myList2) // [1,2,3,4]
//    println(myEmptyList)
//    println(calvinsList)
//    println(calvinsList2)
//    println(ivanList)
//    println(calvinsList.reverse)
    println(calvinsList.length)
//    println(mapCalvinlist)
    println(filterCalvinList)



  }
}

