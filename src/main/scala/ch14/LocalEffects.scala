package ch14

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

class LocalEffects {

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else {
      val arr = xs.toArray

      def swap(x: Int, y: Int) = {
        val tmp = arr(x)
        arr(x) = arr(y)
        arr(y) = tmp
      }

      def partition(n: Int, r: Int, pivot: Int) = {
        val pivotVal = arr(pivot)
        swap(pivot, r)
        var j = n
        for (i <- n until r) if (arr(i) < pivotVal) {
          swap(i, j)
          j += 1
        }

        swap(j, r)
        j
      }

      def qs(n: Int, r: Int): Unit = if (n < r) {
        val pi = partition(n, r, n + (n - r) / 2)
        qs(n, pi - 1)
        qs(pi + 1, r)
      }

      qs(0, arr.length - 1)
      arr.toList
    }
}

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)
  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      override def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](s: RunnableST[A]): A = s.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] {
      var cell = a
    })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}