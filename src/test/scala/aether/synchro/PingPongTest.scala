package aether.synchro

import org.scalatest.FunSuite
import pingpong.PingPong

import scala.io.Source

class PingPongTest extends FunSuite {

  import PingPong._

  test("near racket reflection") {
    val cfg = Configuration(
      tableWidth = 10, tableLength = 500,
      ballSize = 5, ballStartSpeed = 5,
      racketWidth = 10, racketDepth = 5, racketDistanceBack = 100, racketSpeed = 0
    )
    val s0 = State(0, 0, Ball(Point(far = 105, left = 1), Direction(-10, 0)))
    val s1 = smartMove(cfg, s0, RacketDirection(), RacketDirection(), 1)
    assert(s1.isRight)
    for (s1 <- s1.right)
      assert(s1.ball.pos.far > s0.ball.pos.far)
  }

  test("regression - 1") {
    val actual = Example.it()
    val firstDiff = (expectedRegressionData.iterator zip actual).zipWithIndex
      .collectFirst { case ((se, sa), i) if se != sa => (i, se, sa) }
    assert(firstDiff.isEmpty, s"Difference: $firstDiff")
  }

  private lazy val expectedRegressionData: Seq[State] = {
    val Format = raw"State\((.*), (.*), Ball\(Point\((.*), (.*)\), Direction\((.*), (.*)\)\)\)".r
    val source = Source.fromURL(getClass.getResource("/PingPongStates.txt"))
    val lines = source.getLines().toVector
    source.close()
    lines.map { s =>
      val Seq(r1, r2, p1, p2, d1, d2) = Format.unapplySeq(s).toSeq.flatten.map(_.toDouble)
      State(r1, r2, Ball(Point(p1, p2), Direction(d1, d2)))
    }
  }
}
