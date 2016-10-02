package aether.synchro.testdata

import pingpong.PingPong
import PingPong._

import scala.io.Source

object RegressionTestData {
  val cfg = Configuration(
    tableWidth = 300, tableLength = 600,
    ballSize = 10, ballStartSpeed = 10,
    racketWidth = 300, racketDepth = 20, racketDistanceBack = 30
  )

  val s0 = {
    val distToLeft = (cfg.tableWidth - cfg.racketWidth) / 2.0
    State(distToLeft, distToLeft, kickoff(cfg, Normal.apply(Direction(1, 1))))
  }

  val expected: Seq[State] = {
    val Format = raw"State\((.*), (.*), Ball\(Point\((.*), (.*)\), Direction\((.*), (.*)\)\)\)".r
    val source = Source.fromURL(getClass.getResource("/PingPongStates.txt"))
    val lines = source.getLines().toVector
    source.close()
    lines.map { s =>
      val Seq(r1, r2, p1, p2, d1, d2) = Format.unapplySeq(s).toSeq.flatten.map(_.toDouble)
      State(r1, r2, Ball(Point(p1, p2), Direction(d1, d2)))
    }
  }

  val dt: Double = 1.0

  val racketHandler: RacketHandler = (_, _) => RacketDirection()
}
