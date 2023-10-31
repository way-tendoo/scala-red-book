package redbook.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[+A] {
  private[parallelism] def apply(a: A => Unit): Unit
}

object Par {
  private type Par[+A] = ExecutorService => Future[A]

  implicit class ParOps[A](p: Par[A]) {

    def flatMap[B](f: A => Par[B]): Par[B] = { es =>
      new Future[B] {
        def apply(cb: B => Unit): Unit = p(es)(a => f(a)(es)(cb))
      }
    }

    def map[B](f: A => B): Par[B] = flatMap(a => unit(f(a)))

    def run(es: ExecutorService): A = {
      val ref   = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a =>
        ref.set(a); latch.countDown()
      }
      latch.await()
      ref.get()
    }
  }

  def join[A](p: Par[Par[A]]): Par[A] = p.flatMap(identity)

  def unit[A](a: A): Par[A] = { (_: ExecutorService) =>
    new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }
  }

  def fork[A](a: => Par[A]): Par[A] = { es =>
    new Future[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  private def eval(es: ExecutorService)(r: => Unit): Unit = {
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })
  }
}
