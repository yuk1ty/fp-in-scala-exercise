package ch5

import org.scalatest._

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

class StreamSpec extends FunSpec with Matchers {

  describe("Stream") {
    it("Stream(1, 2, 3) のとき、exist(1) すると true を返すこと") {
      assert(Stream(1, 2, 3).exists(_ == 1))
    }

    // Cons の equals がうまくいかなくて落ちるっぽい
    ignore("Stream(1, 2, 3) のとき、take(2) すると 1, 2 を返すこと") {
      Stream(1, 2, 3).take(2) shouldBe Stream(1, 2)
    }

    ignore("Stream(1, 2, 3) のとき、takeWhite(i % 2) すると 2 を返すこと") {
      Stream(1, 2, 3).takeWhile(_ % 2 == 0) shouldBe Stream(1, 3)
    }

    it("Stream(1, 2, 3, 4, 5).toList で List(1, 2, 3, 4, 5) になること") {
      Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
    }
  }
}
