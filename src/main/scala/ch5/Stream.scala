package ch5

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

trait Stream[+A] {

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _          => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0  => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 0 => Stream.cons(h(), Empty)
    case _                    => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _                    => Stream.empty
  }

  // まず最初に true を用意して、
  // 1つ1つ右に要素を見ていって、それぞれの判定を記録していく。
  def forall(p: A => Boolean): Boolean = foldRight(true)((h, t) => p(h) && t)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
      if (p(h)) Stream.cons(h, t) else Stream.empty[A])

  def headOption2: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))
}

case object Empty extends Stream[Nothing]

// 明示的な強制を必要とする thunk
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // コンスセルを作成する
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // 空のストリーム。
  def empty[A]: Stream[A] = Empty

  // 複数の要素から Stream を作成するための、可変長の引数を持つ便利なメソッド。
  def apply[A](as: A*): Stream[A] =
    // cons の引数が Scala によって thunk にまとめられるため、
    // Stream が強制的に評価されるまで、as.head と apply は評価されない。
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
