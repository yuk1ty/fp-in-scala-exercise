package ch7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

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

object Nonblocking {

  type Par[+A] = ExecutorService => Future[A]

  sealed trait Future[A] {
    private[ch7] def apply(k: A => Unit): Unit
  }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown()
    }
    // When we receive the value, sets the result and released the latch.
    latch.await
    // Waits untils the reuslt becomes available and the latch is released.
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es =>
      new Future[A] {
        override private[ch7] def apply(k: (A) => Unit) = k(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        // eval(es)(a(es).apply(k))
        //          ^^^^^
        //          Future
        //                ^^^^^^^^
        //                Future#apply(A => Unit)
        override def apply(k: (A) => Unit): Unit = eval(es)(a(es)(k))
    }

  private def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })

}
