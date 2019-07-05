package net.akouryy.kreins.game

object Pos {
  def rotate180(p: Int) = {
    ~p & 63

    // 0,0->7,7; 0,1->7,6; 4,2->3,5
  }

  def mirrorWithDiagRightUp(p: Int) = {
    (~p & 7) << 3 | ~p >> 3 & 7

    // 0,0->7,7; 0,1->6,7; 4,2->5,3
  }

  def mirrorWithDiagRightDown(p: Int) = {
    (p & 7) << 3 | p >> 3

    // 0,0->0,0; 0,1->1,0; 4,2->2,4
  }
}
