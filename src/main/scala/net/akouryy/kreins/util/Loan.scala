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
    implicit val closeCloser: Closer[ {def close(): Unit}] = _.close()
  }

}
