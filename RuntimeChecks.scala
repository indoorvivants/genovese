package genovese

trait RuntimeChecks:
  def intBoundsCheck(
      start: Int,
      value: Int,
      max: Int
  ): Unit

  def floatBoundsCheck(
      start: Float,
      value: Float,
      max: Float
  ): Unit

  def check(f: => Option[String]): Unit
end RuntimeChecks

transparent inline def runtimeChecks = compiletime.summonInline[RuntimeChecks]

object RuntimeChecks:
  object Full extends RuntimeChecks:
    inline def intBoundsCheck(
        start: Int,
        value: Int,
        max: Int
    ): Unit =
      if value >= start && value <= max then ()
      else sys.error(s"Value $value is out of bounds for [$start, $max]")

    inline def floatBoundsCheck(
        start: Float,
        value: Float,
        max: Float
    ): Unit =
      if value >= start && value <= max then ()
      else sys.error(s"Value $value is out of bounds for [$start, $max]")

    override inline def check(f: => Option[String]): Unit =
      f match
        case scala.None  => ()
        case Some(value) => sys.error(value)

  end Full

  object None extends RuntimeChecks:
    inline def intBoundsCheck(
        start: Int,
        value: Int,
        max: Int
    ): Unit = ()

    inline def floatBoundsCheck(
        start: Float,
        value: Float,
        max: Float
    ): Unit = ()

    override inline def check(f: => Option[String]): Unit = ()
  end None
end RuntimeChecks
