package net.akouryy.kreins
package fl

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket

import util.Loan

case class Connector(host: String, port: Int) {
  def connect(
    init: (String => Unit) => Unit
  )(
    f: (String, String => Unit) => Boolean
  ) = {
    Loan(new Socket(host, port)) { sc =>
      Loan(new BufferedReader(new InputStreamReader(sc.getInputStream))) { reader =>
        Loan(new PrintWriter(sc.getOutputStream, true)) { writer =>
          val output = (s: String) => {
            println(s"sending: '$s'")
            writer.println(s)
          }
          init(output)
          var l = ""
          do {
            l = reader.readLine()
            println(s"received: '$l'")
          } while(f(l, output))
        }
      }
    }
  }
}
