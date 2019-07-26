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

    import scala.language.reflectiveCalls

    implicit val closeCloser: Closer[ {def close(): Unit}] = _.close()
  }

}
