package net.akouryy.kreins.util

import java.io.{InputStream, OutputStream}

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
    implicit val sourceCloser: Closer[io.Source] = _.close()

    implicit val inputStreamCloser: Closer[InputStream] = _.close()
    implicit val outputStreamCloser: Closer[OutputStream] = _.close()
  }

}
