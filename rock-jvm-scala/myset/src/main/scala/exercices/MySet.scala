package exercices

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  def apply(elem: A) : Boolean = contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def -(elem: A) : MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def &(anotherSet: MySet[A]) : MySet[A]
  def --(anotherSet: MySet[A]) : MySet[A]
  def unary_! : MySet[A]
}

class PropertyBaseSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MySet[A] = new PropertyBaseSet[A](x => elem == x || property(x) )

  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBaseSet[A](x => anotherSet(x) || property(x))

  override def -(elem: A): MySet[A] = filter(x => x != elem)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBaseSet[A]( x => property(x) && predicate(x))

  override def unary_! : MySet[A] = new PropertyBaseSet[A](x => !property(x))

  override def map[B](f: A => B): MySet[B] = polityFail
  override def flatMap[B](f: A => MySet[B]): MySet[B] = polityFail
  override def foreach(f: A => Unit): Unit = polityFail

  def polityFail = throw new IllegalArgumentException("Really deep rabbit role")
}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean =  false

  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def -(elem: A): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBaseSet[A](x => true)
}

class NonEmptySet[A](val head: A, val tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if(this contains elem) this
    else new NonEmptySet[A](elem, this)

  /*
  [1 2 3 4] ++ [5 6]
  [2 3 4] ++ [5 6] + 1
  [3 4] ++ [5 6] + 1 + 2
  [4] ++ [5 6] + 1 + 2 + 3
  [] ++ [5 6] + 1 + 2 + 3 + 4
  [5 6] + 1 + 2 + 3 + 4
  [1 5 6] + 2 + 3 + 4
  [2 1 5 6] + 3 + 4
  [3 2 1 5 6] + 4
  [4 3 2 1 5 6]
  */
  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = (tail map f)  + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filterTail = tail filter predicate
    if(predicate(head)) filterTail + head
    else filterTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(elem: A): MySet[A] =
    if(head == elem) tail
    else tail - elem + head

  override def &(anotherSet: MySet[A]): MySet[A] = this filter (anotherSet)

  override def --(anotherSet: MySet[A]): MySet[A] =  this filter !anotherSet

  override def unary_! : MySet[A] = new PropertyBaseSet[A](x => !contains(x))
}

object MySet {
  def apply[A](values: A*) : MySet[A] = {
    @tailrec
    def buildSet(seq: Seq[A], acc: MySet[A]) : MySet[A] = if(seq.isEmpty) acc
    else buildSet(seq.tail, acc + seq.head)
    buildSet(values, MySet.empty())
  }

  def empty[A]() = new EmptySet[A]();
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4, 5)
  MySet(1, 2, 3, 4) ++ MySet(5, 6) foreach print

  println("")
  println("__________________________________")
  MySet(1, 2, 3, 4) foreach print

  println("")
  println("__________________________________")

  MySet(3, 1, 10, 20) & MySet(1, 2, 3, 4) foreach print

  println("")
  println("__________________________________")

  MySet(3, 1, 10, 20) --  MySet(1, 2, 3, 4) foreach print


  println("")
  println("__________________________________")

  val negative = !s;
  println(negative(3))
  println(negative(6))
  println((negative ++ MySet(3))(3))
  "HHmsxIWLa4gT11E"
}
