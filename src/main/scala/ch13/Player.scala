package ch13

import ch11.Monad

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

case class Player(name: String, score: Int)

trait NotPurePlayer {
  def contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score) {
      println(s"${p1.name} is the winner!")
    } else {
      println("It's a draw.")
    }
}

trait PurePlayer {
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String =
    p map {
      case Player(name, _) => s"$name is the winner!"
    } getOrElse "It's a draw."

  def contest(p1: Player, p2: Player): Unit = winner(p1, p2) match {
    case Some(Player(name, _)) => println(s"$name is the winner!")
    case None                  => println("It's a draw.")
  }

  def contest2(p1: Player, p2: Player): Unit =
    println(winnerMsg(winner(p1, p2)))
}

trait PurePlayerWiIO {

  sealed trait IO { self =>
    def run: Unit

    def ++(io: IO): IO = new IO {
      override def run: Unit = {
        self.run
        io.run
      }
    }
  }

  object IO {
    def empty: IO = new IO {
      override def run: Unit = ()
    }
  }

  def printLine(msg: String): IO = new IO {
    override def run: Unit = println(msg)
  }

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String =
    p map {
      case Player(name, _) => s"$name is the winner!"
    } getOrElse "It's a draw."

  def contest(p1: Player, p2: Player): IO =
    printLine(winnerMsg(winner(p1, p2)))
}

sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
    override def run: B = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run: B = f(self.run).run
  }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] {
    override def run: A = a
  }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)
}