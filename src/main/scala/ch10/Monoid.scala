package ch10

import ch3.{Branch, Leaf, Tree}

/*
 * Copyright 2017 Yuki Toyoda
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 0
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1.compose(a2)
    override def zero: (A) => A = a => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((a, b) => m.op(a, f(b)))

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(l), Stub(r))       => Stub(l + r)
      case (Stub(s), Part(l, c, r)) => Part(s + l, c, r)
      case (Part(l, c, r), Stub(s)) => Part(l, c, s + r)
      case (Part(l1, c1, r1), Part(l2, c2, r2)) =>
        Part(l1, c1 + (if ((r1 + l2).isEmpty) 0 else 1) + c2, r2)
    }
    override def zero: WC = Stub("")
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def zero: A = m.zero
    override def op(a1: A, a2: A): A = m.op(a1, a2)
  }

  // def count

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  }

  trait ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  trait IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  trait StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  trait TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(a)      => f(a, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(a)      => f(a, z)
        case Branch(l, r) => foldLeft(l)(foldLeft(l)(z)(f))(f)
      }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }
  }
}
