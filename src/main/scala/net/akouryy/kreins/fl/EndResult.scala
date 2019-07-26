package net.akouryy.kreins
package fl

sealed trait EndResult

object EndResult {

  case object Win extends EndResult

  case object Lose extends EndResult

  case object Draw extends EndResult

}
