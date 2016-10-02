package pingpong

import ungeneric.Constrained

import scala.annotation.tailrec
import scala.util.Random

object PingPong {

  final case class Point(far: Double, left: Double) {
    def +(dir: Direction): Point = {
      Point(far = far + dir.df, left = left + dir.dl)
    }
  }

  final case class Direction(df: Double, dl: Double) {
    def +(that: Direction): Direction = {
      Direction(df = this.df + that.df, dl = this.dl + that.dl)
    }

    def -(that: Direction): Direction = {
      Direction(df = this.df - that.df, dl = this.dl - that.dl)
    }

    def *(that: Direction): Double = {
      this.df * that.df + this.dl * that.dl
    }

    def *(k: Double): Direction = {
      Direction(df = k * df, dl = k * dl)
    }

    def /(k: Double): Direction = {
      Direction(df = this.df / k, dl = this.dl / k)
    }

    def norm: Double = {
      math.sqrt(Seq(dl, df).map(math.pow(_, 2)).sum)
    }

    def normalize: Direction = this / norm
  }

  implicit final class DirectionSwapMethods(val value: Double) extends AnyVal {
    def *(direction: Direction): Direction = direction * value
  }

  val Normal = Constrained[Direction => ?, Direction](_.normalize)

  final case class Configuration(
    tableWidth: Int, tableLength: Int,
    ballSize: Int, ballStartSpeed: Double,
    racketWidth: Int, racketDepth: Int, racketDistanceBack: Int)

  final case class RacketDirection(dl: Double = 0.0) {
    def toDirection: Direction = Direction(df = 0, dl = dl)
  }

  final case class Ball(pos: Point, dir: Direction) {
    def move(dt: Double): Ball = copy(pos = pos + dt * dir)
  }

  final case class State(nearRacketDistToLeft: Double, farRacketDistToLeft: Double, ball: Ball)

  def reflect(d: Direction, n: Normal.Value): Direction = d - 2 * (d * n) * n

  final type RacketHandler = (Configuration, State) => RacketDirection

  def consistent(c: Configuration, s: State): Boolean =
    collisions(traces(c, s, RacketDirection(), RacketDirection(), 0)).isEmpty

  final case class Line(x: Point, y: Point)

  sealed trait ObjectId

  case object NearRacketFrontId extends ObjectId

  case object NearRacketLeftSideId extends ObjectId

  case object NearRacketRightSideId extends ObjectId

  case object FarRacketFrontId extends ObjectId

  case object FarRacketLeftSideId extends ObjectId

  case object FarRacketRightSideId extends ObjectId

  case object BallId extends ObjectId

  case object LeftBorderId extends ObjectId

  case object RightBorderId extends ObjectId

  case object NearGoalLineId extends ObjectId

  case object FarGoalLineId extends ObjectId

  final case class Trace(lines: Seq[Line])

  def createTrace(points: Point*): Trace = {
    val lines = points match {
      case Seq(p1, p2) =>
        Seq(Line(p1, p2))
      case _ if points.length > 2 =>
        points.zip(points.tail :+ points.head).map(Line.tupled)
    }
    Trace(lines)
  }

  def partition(t0: Double, t1: Double, eps: Double, predicate: Double => Boolean): Double = {
    @tailrec
    def loop(t0: Double, t1: Double): Double = {
      require(predicate(t0) && !predicate(t1))
      if ((t0 - t1).abs < eps) {
        t0
      } else {
        val middle = (t0 + t1) / 2
        if (predicate(middle)) loop(middle, t1)
        else loop(t0, middle)
      }
    }
    if (!predicate(t0)) t0
    else if (predicate(t1)) t1
    else loop(t0, t1)
  }

  def staticTraces(cfg: Configuration): Map[ObjectId, Trace] = {
    import cats.instances.all._
    import cats.syntax.all._

    import scala.collection.breakOut
    val nl = Point(far = -cfg.ballSize, left = 0)
    val nr = Point(far = -cfg.ballSize, left = cfg.tableWidth)
    val fl = Point(far = cfg.tableLength + cfg.ballSize, left = 0)
    val fr = Point(far = cfg.tableLength + cfg.ballSize, left = cfg.tableWidth)
    Seq(
      LeftBorderId -> Seq(nl, fl),
      RightBorderId -> Seq(nr, fr),
      NearGoalLineId -> Seq(nl, nr),
      FarGoalLineId -> Seq(fl, fr)
    ).map(_.map(createTrace))(breakOut)
  }

  object BallVertices {
    def nr(implicit cfg: Configuration): Direction = Direction(df = 0, dl = cfg.ballSize)

    def fl(implicit cfg: Configuration): Direction = Direction(df = cfg.ballSize, dl = 0)

    def fr(implicit cfg: Configuration): Direction = Direction(df = cfg.ballSize, dl = cfg.ballSize)
  }

  def ballTrace(p1: Point, p2: Point)(implicit cfg: Configuration): Trace = {
    val Seq(n, f) = Seq(p1, p2).sortBy(p => (p.far, p.left))
    import BallVertices._
    (n, f) match {
      case _ if f.far == n.far && f.left >= n.left =>
        createTrace(n, n + fl, f + fr, f + nr)
      case _ if f.left == n.left && f.far > n.far =>
        createTrace(n + nr, n, f + fl, f + fr)
      case _ if f.left < n.left && f.far > n.far =>
        createTrace(n + fr, n + nr, n, f, f + fl, f + fr)
      case _ if f.left > n.left && f.far > n.far =>
        createTrace(n + nr, n, n + fl, f + fl, f + fr, f + nr)
    }
  }

  object RacketVertices {
    def nlNear(left: Double)(implicit cfg: Configuration): Point =
      Point(far = cfg.racketDistanceBack, left = left)

    def nlFar(left: Double)(implicit cfg: Configuration): Point =
      Point(far = cfg.tableLength - cfg.racketDistanceBack - cfg.racketDepth, left = left)

    def nr(implicit cfg: Configuration): Direction = Direction(df = 0, dl = cfg.racketWidth)

    def fl(implicit cfg: Configuration): Direction = Direction(df = cfg.racketDepth, dl = 0)

    def fr(implicit cfg: Configuration): Direction = Direction(df = cfg.racketDepth, dl = cfg.racketWidth)
  }

  def nearRacketTrace(distToLeft1: Double, distToLeft2: Double)(implicit cfg: Configuration): Map[ObjectId, Trace] = {
    import RacketVertices._
    val Seq(nl1, nl2) = Seq(distToLeft1, distToLeft2).sorted.map(nlNear)
    Map(
      NearRacketFrontId -> createTrace(nl1 + fl, nl2 + fr),
      NearRacketLeftSideId -> createTrace(nl1, nl1 + fl, nl2 + fl, nl2),
      NearRacketRightSideId -> createTrace(nl1 + nr, nl1 + fr, nl2 + fr, nl2 + nr)
    )
  }

  def farRacketTrace(cfg: Configuration, distToLeft1: Double, distToLeft2: Double): Map[ObjectId, Trace] = {
    val Seq(dtl1, dtl2) = Seq(distToLeft1, distToLeft2).sorted
    Map(
      FarRacketFrontId -> createTrace(
        Point(far = cfg.tableLength - cfg.racketDistanceBack - cfg.racketDepth, left = dtl1),
        Point(far = cfg.tableLength - cfg.racketDistanceBack - cfg.racketDepth, left = dtl2 + cfg.racketWidth)
      ),
      FarRacketLeftSideId -> createTrace(
        Point(far = cfg.tableLength - cfg.racketDistanceBack, left = dtl1),
        Point(far = cfg.tableLength - cfg.racketDistanceBack, left = dtl2),
        Point(far = cfg.tableLength - cfg.racketDistanceBack - cfg.racketDepth, left = dtl2),
        Point(far = cfg.tableLength - cfg.racketDistanceBack - cfg.racketDepth, left = dtl1)
      ),
      FarRacketRightSideId -> createTrace(
        Point(far = cfg.tableLength - cfg.racketDistanceBack, left = dtl1 + cfg.racketWidth),
        Point(far = cfg.tableLength - cfg.racketDistanceBack, left = dtl2 + cfg.racketWidth),
        Point(far = cfg.tableLength - cfg.racketDistanceBack - cfg.racketDepth, left = dtl2 + cfg.racketWidth),
        Point(far = cfg.tableLength - cfg.racketDistanceBack - cfg.racketDepth, left = dtl1 + cfg.racketWidth)
      )
    )
  }

  def traces(
    cfg: Configuration, s0: State,
    nearRacket: RacketDirection, farRacket: RacketDirection, dt: Double
  ): Map[ObjectId, Trace] = {
    val s1 = straightMove(cfg, s0, nearRacket, farRacket, dt)
    staticTraces(cfg) ++
      nearRacketTrace(s0.nearRacketDistToLeft, s1.nearRacketDistToLeft)(cfg) ++
      farRacketTrace(cfg, s0.farRacketDistToLeft, s1.farRacketDistToLeft) ++ Seq(
      BallId -> ballTrace(s0.ball.pos, s1.ball.pos)(cfg))
  }

  def ccw(a: Point, b: Point, c: Point): Option[Boolean] = {
    val k = (c.far - a.far) * (b.left - a.left) - (b.far - a.far) * (c.left - a.left)
    if (k == 0) None
    else Some(k > 0)
  }

  def intersect(l1: Line, l2: Line): Boolean = {
    import cats.instances.all._
    import cats.syntax.cartesian._
    val Line(a, b) = l1
    val Line(c, d) = l2
    (ccw(a, c, d) |@| ccw(b, c, d)).map(_ != _).contains(true) &&
      (ccw(a, b, c) |@| ccw(a, b, d)).map(_ != _).contains(true)
  }

  def collisions(shapes: Map[ObjectId, Trace]): Seq[(ObjectId, ObjectId)] = {
    shapes.toSeq
      .combinations(2)
      .collect {
        case Seq((id1, Trace(ls1)), (id2, Trace(ls2))) if ls1.exists(l1 => ls2.exists(intersect(l1, _))) =>
          id1 -> id2
      }
      .toSeq
  }

  val Eps = 1e-6

  def straightMove(
    cfg: Configuration, s: State,
    nearRacket: RacketDirection, farRacket: RacketDirection, dt: Double
  ): State = {
    s.copy(
      nearRacketDistToLeft = s.nearRacketDistToLeft + nearRacket.dl * dt,
      farRacketDistToLeft = s.farRacketDistToLeft + farRacket.dl * dt,
      ball = s.ball.move(dt)
    )
  }

  def kickoff(cfg: Configuration, dir: Normal.Value): Ball = {
    Ball(
      pos = Point(far = cfg.tableLength / 2, left = cfg.tableWidth / 2),
      dir = cfg.ballStartSpeed * dir
    )
  }

  sealed trait CollisionResult

  case object StopNearRacket extends CollisionResult

  case object StopFarRacket extends CollisionResult

  final case class KickOffBall(dir: Normal.Value) extends CollisionResult

  final case class ReflectBall(n: Normal.Value, impulse: Direction = Direction(0, 0)) extends CollisionResult

  final case class DynamicPart(ball: Ball, nearRacket: RacketDirection, farRacket: RacketDirection)

  type CollisionHandler = DynamicPart => CollisionResult

  object Directions {
    val right = Normal.apply(Direction(df = 0, dl = 1))
    val left = Normal.apply(Direction(df = 0, dl = -1))
    val forward = Normal.apply(Direction(df = 1, dl = 0))
    val backward = Normal.apply(Direction(df = -1, dl = 0))
  }

  val collisionHandlers = {
    import Directions._

    val nearRacketStop: CollisionHandler = dp => StopNearRacket
    val nearRacketStops =
      (for {
        s <- Seq(NearRacketFrontId, NearRacketLeftSideId, NearRacketRightSideId)
        b <- Seq(LeftBorderId, RightBorderId)
      } yield {
        Set[ObjectId](s, b) -> nearRacketStop
      }).toMap

    val farRacketStop: CollisionHandler = dp => StopFarRacket
    val farRacketStops =
      (for {
        s <- Seq(FarRacketFrontId, FarRacketLeftSideId, FarRacketRightSideId)
        b <- Seq(LeftBorderId, RightBorderId)
      } yield {
        Set[ObjectId](s, b) -> farRacketStop
      }).toMap

    val ballReflections = Map[Set[ObjectId], CollisionHandler](
      (Set(BallId, NearRacketFrontId), dp => ReflectBall(forward, dp.nearRacket.toDirection)),
      (Set(BallId, NearRacketLeftSideId), dp => ReflectBall(right, dp.nearRacket.toDirection)),
      (Set(BallId, NearRacketRightSideId), dp => ReflectBall(left, dp.nearRacket.toDirection)),

      (Set(BallId, FarRacketFrontId), dp => ReflectBall(backward, dp.farRacket.toDirection)),
      (Set(BallId, FarRacketLeftSideId), dp => ReflectBall(right, dp.farRacket.toDirection)),
      (Set(BallId, FarRacketRightSideId), dp => ReflectBall(left, dp.farRacket.toDirection)),

      (Set(BallId, LeftBorderId), dp => ReflectBall(right)),
      (Set(BallId, RightBorderId), dp => ReflectBall(left)),

      (Set(BallId, NearGoalLineId), dp => KickOffBall(backward)),
      (Set(BallId, FarGoalLineId), dp => KickOffBall(forward))
    )

    ballReflections ++ nearRacketStops ++ farRacketStops
  }

  def handleCollisions(
    cfg: Configuration, collisions: Seq[(ObjectId, ObjectId)],
    ball: Ball, nearRacket: RacketDirection, farRacket: RacketDirection
  ): DynamicPart = {
    val results = collisions.map {
      case (i1, i2) => collisionHandlers(Set(i1, i2))(DynamicPart(ball, nearRacket, farRacket))
    }
    val kickOff = results.collect { case KickOffBall(n) => n } match {
      case Seq() => None
      case Seq(n) => Some(kickoff(cfg, n))
    }
    val reflection = results
      .collect { case ReflectBall(n, i) => reflect(ball.dir, n) + i }
      .reduceOption(_ + _)
      .map(dir => ball.copy(dir = dir))
    DynamicPart(
      ball = kickOff.orElse(reflection).getOrElse(ball),
      nearRacket = if (results.contains(StopNearRacket)) RacketDirection(0) else nearRacket,
      farRacket = if (results.contains(StopFarRacket)) RacketDirection(0) else farRacket
    )
  }

  @tailrec
  def smartMove(
    cfg: Configuration, s0: State,
    nearRacket: RacketDirection, farRacket: RacketDirection, dt: Double
  ): State = {
    val args = (cfg, s0, nearRacket, farRacket, dt)
    require(consistent(cfg, s0), args)
    val getCollisions = (s: State, t: Double) => collisions(traces(cfg, s, nearRacket, farRacket, t))
    if (getCollisions(s0, dt).isEmpty) {
      straightMove(cfg, s0, nearRacket, farRacket, dt).ensuring(consistent(cfg, _), args)
    } else {
      val dt1 = partition(t0 = 0, t1 = dt, eps = Eps, getCollisions(s0, _).isEmpty)
      val s1 = straightMove(cfg, s0, nearRacket, farRacket, dt1)
      val DynamicPart(b2, nearRacket2, farRacket2) =
        handleCollisions(cfg, getCollisions(s1, dt1 + Eps), s1.ball, nearRacket, farRacket)
      smartMove(cfg, s1.copy(ball = b2), nearRacket2, farRacket2, dt - dt1)
    }
  }

  def iterate(
    cfg: Configuration, s0: State,
    nearRacket: RacketHandler, farRacket: RacketHandler,
    dt: Double
  ): Iterator[State] = {
    require(consistent(cfg, s0))
    Iterator.iterate(s0) { s =>
      smartMove(cfg, s, nearRacket(cfg, s), farRacket(cfg, s), dt)
    }
  }

  def barPlanarPoints(a: putpixel.Point, b: putpixel.Point): Seq[putpixel.Point] = {
    for {
      x <- a.x until b.x
      y <- a.y until b.y
    } yield {
      putpixel.Point(x, y)
    }
  }

  def barPoints(a: Point, b: Point): Seq[putpixel.Point] = {
    def round(p: Point) = putpixel.Point(y = p.left.round.toInt, x = p.far.round.toInt)
    barPlanarPoints(round(a), round(b))
  }

  def ballPoints(nl: Point)(implicit cfg: Configuration): Seq[putpixel.Point] = {
    import BallVertices._
    barPoints(nl, nl + fr)
  }

  def nearRacketPoints(left: Double)(implicit cfg: Configuration): Seq[putpixel.Point] = {
    import RacketVertices._
    val nl = nlNear(left)
    barPoints(nl, nl + fr)
  }

  def farRacketPoints(left: Double)(implicit cfg: Configuration): Seq[putpixel.Point] = {
    import RacketVertices._
    val nl = nlFar(left)
    barPoints(nl, nl + fr)
  }

  def points(cfg: Configuration, s: State): Seq[putpixel.Point] = {
    ballPoints(s.ball.pos)(cfg) ++
      nearRacketPoints(s.nearRacketDistToLeft)(cfg) ++
      farRacketPoints(s.farRacketDistToLeft)(cfg)
  }

  def pixels(cfg: Configuration, s: State): Seq[putpixel.Pixel] = {
    points(cfg, s).map(putpixel.Pixel(_, putpixel.Color.White))
  }

  def pixels(cfg: Configuration, s1: State, s2: State): Seq[putpixel.Pixel] = {
    val ps1 = points(cfg, s1).toSet
    val ps2 = points(cfg, s2).toSet
    val black = (ps1 -- ps2).map(putpixel.Pixel(_, putpixel.Color.Black))
    val white = (ps2 -- ps1).map(putpixel.Pixel(_, putpixel.Color.White))
    black.toSeq ++ white
  }

  def randomRacketHandler(cfg: Configuration, state: State): RacketDirection = {
    RacketDirection(Seq(0, -10, 10)(Random.nextInt(3)))
  }

  def statesToPixels(cfg: Configuration, ss: Iterator[State]): Iterator[Seq[putpixel.Pixel]] = {
    var s0 = ss.next()
    Iterator(pixels(cfg, s0)) ++ ss.map { s1 =>
      val pxs = pixels(cfg, s0, s1)
      s0 = s1
      pxs
    }
  }

  object Example {
    val cfg = Configuration(
      tableWidth = 300, tableLength = 600,
      ballSize = 10, ballStartSpeed = 10,
      racketWidth = 300, racketDepth = 20, racketDistanceBack = 30
    )

    val s0 = {
      val distToLeft = (cfg.tableWidth - cfg.racketWidth) / 2.0
      State(distToLeft, distToLeft, kickoff(cfg, Normal.apply(Direction(1, 1))))
    }

    def draw(): Unit = {
      val ss = iterate(cfg, s0, (_, _) => RacketDirection(), (_, _) => RacketDirection(), dt = 1)
      val pxs = statesToPixels(cfg, ss)
      val pp = putpixel.PutPixel.start(width = cfg.tableLength, height = cfg.tableWidth)
      for (ps <- pxs) {
        ps.foreach(pp)
        Thread.sleep(10)
      }
    }
  }

}
