package net.akouryy.kreins.util

object Loan {
  def apply[A, B](resource: A)(body: A => B)(implicit closer: Closer[A]): B = {
    try {
      body(resource)
    } finally {
      closer(resource)
    }
  }

  trait Closer[-A] {
    def apply(resource: A)
  }

  object Closer {
    implicit val sourceCloser: Closer[io.Source] = s => s.close
  }

}
