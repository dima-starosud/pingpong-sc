package putpixel

final case class Color(r: Int = 0, g: Int = 0, b: Int = 0)

object Color {
  val Black: Color = Color()
  val White: Color = Color(255, 255, 255)
}
