package net.akouryy.kreins
package fl

sealed trait EndResult

object EndResult {

  final case object Win extends EndResult

  final case object Lose extends EndResult

  final case object Tie extends EndResult

}
