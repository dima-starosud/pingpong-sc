package aether.synchro

import org.scalatest.FunSuite
import pingpong.PingPong

class PingPongTest extends FunSuite {

  import PingPong._

  test("near racket reflection") {
    val cfg = Configuration(
      tableWidth = 10, tableLength = 500,
      ballSize = 5, ballStartSpeed = 5,
      racketWidth = 10, racketDepth = 5, racketDistanceBack = 100
    )
    val s0 = State(0, 0, Ball(Point(far = 105, left = 1), Direction(-10, 0)))
    val s1 = smartMove(cfg, s0, RacketDirection(), RacketDirection(), 1)
    assert(s1.ball.pos.far > s0.ball.pos.far)
  }

  test("regression - 1") {
    import testdata.RegressionTestData._
    val actual = iterate(cfg, s0, racketHandler, racketHandler, dt)
    val firstDiff = (expected.iterator zip actual).zipWithIndex
      .collectFirst { case ((se, sa), i) if se != sa => (i, se, sa) }
    assert(firstDiff.isEmpty, s"Difference: $firstDiff")
  }
}
