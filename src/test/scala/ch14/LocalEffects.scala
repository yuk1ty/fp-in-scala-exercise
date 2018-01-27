package ch14

import org.scalatest.{FunSpec, Matchers}

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

class LocalEffects extends FunSpec with Matchers {

  describe("ST, STRef") {
    it("for-yield 内で ST と STRef を動かせること") {
      for {
        // わざと発散型を選んでいる
        r1 <- STRef[Nothing, Int](1)
        r2 <- STRef[Nothing, Int](1)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }
  }

  describe("RunnableST") {
    it("動くこと") {
      val p = new RunnableST[(Int, Int)] {
        override def apply[S]: ST[S, (Int, Int)] = for {
          r1 <- STRef(1)
          r2 <- STRef(2)
          x <- r1.read
          y <- r2.read
          _ <- r1.write(y + 1)
          _ <- r2.write(x + 1)
          a <- r1.read
          b <- r2.read
        } yield (a, b) // expects (3, 2)
      }
    }
  }
}
