package pingpong

import ungeneric.Constrained

import scala.annotation.tailrec

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
    racketWidth: Int, racketDepth: Int, racketDistanceBack: Int, racketSpeed: Double)

  final case class RacketDirection(dl: Double = 0.0) {
    def toDirection: Direction = Direction(df = 0, dl = dl)
  }

  final case class Ball(pos: Point, dir: Direction) {
    def move(dt: Double): Ball = copy(pos = pos + dt * dir)
  }

  final case class State(nearRacketDistToLeft: Double, farRacketDistToLeft: Double, ball: Ball)

  def reflect(d: Direction, n: Normal.Value): Direction = d - 2 * (d * n) * n

  final type RacketHandler = (Configuration, State) => RacketDirection
  final type KickoffHandler = (Configuration, Option[EndOfRound]) => State

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

  final case class Trace(lines: Seq[Line]) extends AnyVal

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
    require(predicate(t0))
    if (predicate(t1)) t1
    else loop(t0, t1)
  }

  def staticTraces(cfg: Configuration): Map[ObjectId, Trace] = {
    val nl = Point(far = -cfg.ballSize, left = 0)
    val nr = Point(far = -cfg.ballSize, left = cfg.tableWidth)
    val fl = Point(far = cfg.tableLength + cfg.ballSize, left = 0)
    val fr = Point(far = cfg.tableLength + cfg.ballSize, left = cfg.tableWidth)
    Map(
      LeftBorderId -> createTrace(nl, fl),
      RightBorderId -> createTrace(nr, fr),
      NearGoalLineId -> createTrace(nl, nr),
      FarGoalLineId -> createTrace(fl, fr)
    )
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

  def farRacketTrace(distToLeft1: Double, distToLeft2: Double)(implicit cfg: Configuration): Map[ObjectId, Trace] = {
    import RacketVertices._
    val Seq(nl1, nl2) = Seq(distToLeft1, distToLeft2).sorted.map(nlFar)
    Map(
      FarRacketFrontId -> createTrace(nl1, nl2 + nr),
      FarRacketLeftSideId -> createTrace(nl1, nl1 + fl, nl2 + fl, nl2),
      FarRacketRightSideId -> createTrace(nl1 + nr, nl1 + fr, nl2 + fr, nl2 + nr)
    )
  }

  def traces(
    cfg: Configuration, s0: State,
    nearRacket: RacketDirection, farRacket: RacketDirection, dt: Double
  ): Map[ObjectId, Trace] = {
    val s1 = straightMove(cfg, s0, nearRacket, farRacket, dt)
    staticTraces(cfg) ++
      nearRacketTrace(s0.nearRacketDistToLeft, s1.nearRacketDistToLeft)(cfg) ++
      farRacketTrace(s0.farRacketDistToLeft, s1.farRacketDistToLeft)(cfg) ++ Seq(
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

  sealed trait RacketId {
    def fold[T](near: => T, far: => T): T
  }

  case object NearRacketId extends RacketId {
    override def fold[T](near: => T, far: => T): T = near
  }

  case object FarRacketId extends RacketId {
    override def fold[T](near: => T, far: => T): T = far
  }

  sealed trait CollisionResult

  case object StopNearRacket extends CollisionResult

  case object StopFarRacket extends CollisionResult

  final case class ReflectBall(n: Normal.Value, racket: Option[RacketId] = None) extends CollisionResult

  final case class DynamicPart(ball: Ball, nearRacket: RacketDirection, farRacket: RacketDirection)

  object Directions {
    val right = Normal.apply(Direction(df = 0, dl = 1))
    val left = Normal.apply(Direction(df = 0, dl = -1))
    val forward = Normal.apply(Direction(df = 1, dl = 0))
    val backward = Normal.apply(Direction(df = -1, dl = 0))
  }

  val collisionResults: Map[Set[ObjectId], CollisionResult] = {
    import Directions._

    val nearRacketStops =
      for {
        s <- Seq(NearRacketFrontId, NearRacketLeftSideId, NearRacketRightSideId)
        b <- Seq(LeftBorderId, RightBorderId)
      } yield {
        Set[ObjectId](s, b) -> StopNearRacket
      }

    val farRacketStops =
      for {
        s <- Seq(FarRacketFrontId, FarRacketLeftSideId, FarRacketRightSideId)
        b <- Seq(LeftBorderId, RightBorderId)
      } yield {
        Set[ObjectId](s, b) -> StopFarRacket
      }

    val ballReflections = Map[Set[ObjectId], CollisionResult](
      (Set(BallId, NearRacketFrontId), ReflectBall(forward, Some(NearRacketId))),
      (Set(BallId, NearRacketLeftSideId), ReflectBall(right, Some(NearRacketId))),
      (Set(BallId, NearRacketRightSideId), ReflectBall(left, Some(NearRacketId))),

      (Set(BallId, FarRacketFrontId), ReflectBall(backward, Some(FarRacketId))),
      (Set(BallId, FarRacketLeftSideId), ReflectBall(right, Some(FarRacketId))),
      (Set(BallId, FarRacketRightSideId), ReflectBall(left, Some(FarRacketId))),

      (Set(BallId, LeftBorderId), ReflectBall(right)),
      (Set(BallId, RightBorderId), ReflectBall(left))
    )

    val endsOfRound = Map[Set[ObjectId], EndOfRound](
      (Set(BallId, NearGoalLineId), BallCrossedNearGoalLine),
      (Set(BallId, FarGoalLineId), BallCrossedFarGoalLine)
    )

    ballReflections ++ nearRacketStops ++ farRacketStops ++ endsOfRound
  }

  def handleCollisions(
    cfg: Configuration, collisions: Seq[(ObjectId, ObjectId)],
    ball: Ball, nearRacket: RacketDirection, farRacket: RacketDirection
  ): Either[EndOfRound, DynamicPart] = {
    val results = collisions.map {
      case (i1, i2) => collisionResults(Set(i1, i2))
    }
    val kickOff = results.collect { case eor: EndOfRound => eor } match {
      case Seq() => None
      case Seq(n) => Some(n)
    }
    val reflection = results
      .collect {
        case ReflectBall(n, r) =>
          reflect(ball.dir, n) + r.fold(Direction(0, 0))(_.fold(nearRacket, farRacket).toDirection)
      }
      .reduceOption(_ + _)
      .map(dir => ball.copy(dir = dir))
    kickOff.fold[Either[EndOfRound, DynamicPart]](Right(DynamicPart(
      ball = reflection.getOrElse(ball),
      nearRacket = if (results.contains(StopNearRacket)) RacketDirection(0) else nearRacket,
      farRacket = if (results.contains(StopFarRacket)) RacketDirection(0) else farRacket
    )))(Left(_))
  }

  sealed trait EndOfRound extends CollisionResult

  case object BallCrossedNearGoalLine extends EndOfRound

  case object BallCrossedFarGoalLine extends EndOfRound

  def smartMove(
    cfg: Configuration, s0: State,
    nearRacket: RacketDirection, farRacket: RacketDirection, dt: Double
  ): Either[EndOfRound, State] = {
    val args = (cfg, s0, nearRacket, farRacket, dt)
    require(consistent(cfg, s0), args)
    val getCollisions = (s: State, t: Double) => collisions(traces(cfg, s, nearRacket, farRacket, t))
    if (getCollisions(s0, dt).isEmpty) {
      Right(straightMove(cfg, s0, nearRacket, farRacket, dt).ensuring(consistent(cfg, _), args))
    } else {
      val dt1 = partition(t0 = 0, t1 = dt, eps = Eps, getCollisions(s0, _).isEmpty)
      val s1 = straightMove(cfg, s0, nearRacket, farRacket, dt1)
      handleCollisions(cfg, getCollisions(s1, dt1 + Eps), s1.ball, nearRacket, farRacket).right.flatMap {
        case DynamicPart(b2, nearRacket2, farRacket2) =>
          smartMove(cfg, s1.copy(ball = b2), nearRacket2, farRacket2, dt - dt1)
      }
    }
  }

  def iterate(
    cfg: Configuration,
    kickoff: KickoffHandler,
    nearRacket: RacketHandler, farRacket: RacketHandler,
    dt: Double
  ): Iterator[State] = {
    val s0 = kickoff(cfg, None)
    require(consistent(cfg, s0))
    Iterator.iterate(s0) { s0 =>
      val s1 = smartMove(cfg, s0, nearRacket(cfg, s0), farRacket(cfg, s0), dt)
      s1.left.map(eor => kickoff(cfg, Some(eor))).merge
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

  def ballLeftRacketHandler(racketId: RacketId, cfg: Configuration, state: State): RacketDirection = {
    val current = racketId.fold(state.nearRacketDistToLeft, state.farRacketDistToLeft)
    val target = state.ball.pos.left
    RacketDirection((target - current).signum * cfg.racketSpeed)
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
      racketWidth = 300, racketDepth = 20, racketDistanceBack = 30, racketSpeed = 0
    )
    val centeredRacket = (cfg.tableWidth - cfg.racketWidth) / 2.0

    def ko(cfg: Configuration, eor: Option[EndOfRound]): State = {
      require(eor.isEmpty)
      State(centeredRacket, centeredRacket, kickoff(cfg, Normal.apply(Direction(1, 1))))
    }

    def it() = iterate(cfg, ko, (_, _) => RacketDirection(), (_, _) => RacketDirection(), dt = 1)

    def draw(): Unit = {
      val ss = it()
      val pxs = statesToPixels(cfg, ss)
      val pp = putpixel.PutPixel.start(width = cfg.tableLength, height = cfg.tableWidth)
      for (ps <- pxs) {
        ps.foreach(pp)
        Thread.sleep(10)
      }
    }
  }

}
