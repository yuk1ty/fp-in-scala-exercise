package ch6

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

trait RNG {

  def nextInt: (Int, RNG)

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {

    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // unit は単位元。
  // RNG を未使用のまま渡すのと、定数を返す。
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((h, t) => map2(h, t)(_ :: _))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (d, r) = nonNegativeInt(rng)
    (d / (Int.MaxValue.toDouble + 1), r)
  }

  def _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def inner(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (xs, r)
      } else {
        val (x, r2) = r.nextInt
        inner(count - 1, r2, x :: xs)
      }
    }

    inner(count, rng, List())
  }

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = {
      flatMap(a => State.unit(f(a)))
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B](s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  object State {

    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] = State((a, _))
  }

}
