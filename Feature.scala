package genovese

private class CategoryCalc(
    val steps: Map[Int, Float],
    val stepSize: Float
)
private object CategoryCalc:
  def apply(alts: Seq[Int]) =
    val cnt  = alts.size
    val step = 1.0f / cnt

    new CategoryCalc(
      List.tabulate(alts.size)(idx => idx -> step * idx).toMap,
      step
    )

enum Feature[T]:
  case FloatRange(from: Float, to: Float) extends Feature[Float]
  case IntRange(from: Int, to: Int)       extends Feature[Int]
  case NormalisedFloatRange               extends Feature[NormalisedFloat]
  case Bool                               extends Feature[Boolean]
  case StringCategory(alts: List[String]) extends Feature[String]
  case IntCategory(alts: List[Int])       extends Feature[Int]
  case Optional(feature: Feature[T])      extends Feature[Option[T]]

  private lazy val categorySteps: CategoryCalc | Null =
    this match
      case StringCategory(alts) =>
        CategoryCalc(alts.indices)
      case IntCategory(alts) =>
        CategoryCalc(alts)
      case _ => null

  def toNormalisedFloat(value: this.type#T)(using
      RuntimeChecks
  ): NormalisedFloat =
    import NormalisedFloat.*
    this match
      case FloatRange(from, to) =>
        runtimeChecks.floatBoundsCheck(from, value, to)
        NormalisedFloat((value - from) / (to - from))

      case IntRange(from, to) =>
        runtimeChecks.intBoundsCheck(from, value, to)
        NormalisedFloat((value - from).toFloat / (to - from).toFloat)

      case Bool =>
        if value then ONE else ZERO

      case NormalisedFloatRange =>
        value

      case StringCategory(alts) =>
        val idxof = alts.indexOf(value)

        runtimeChecks.check(
          Option.when(idxof == -1)(
            s"Value of [$value] not found among alternatives [${alts.mkString(", ")}]"
          )
        )

        NormalisedFloat(
          categorySteps.nn.steps(
            idxof
          ) + categorySteps.nn.stepSize / 2
        )

      case IntCategory(alts) =>
        val idxof = alts.indexOf(value)

        runtimeChecks.check(
          Option.when(idxof == -1)(
            s"Value of [$value] not found among alternatives [${alts.mkString(", ")}]"
          )
        )

        NormalisedFloat(
          categorySteps.nn.steps(
            idxof
          ) + categorySteps.nn.stepSize / 2
        )
      case Optional(what) =>
        value match
          case None => NormalisedFloat(0.25f)
          case Some(value) =>
            NormalisedFloat(what.toNormalisedFloat(value) / 2 + 0.5f)

    end match
  end toNormalisedFloat

  def fromNormalisedFloat(value: NormalisedFloat): this.type#T =
    this match
      case FloatRange(from, to) =>
        from + value * (to - from)
      case IntRange(from, to) =>
        (from + value * (to - from)).toInt
      case Bool                 => value >= 0.5f
      case NormalisedFloatRange => value

      case StringCategory(alts) =>
        val modulo = (value / categorySteps.nn.stepSize).toInt
        alts(modulo.min(alts.length - 1))

      case IntCategory(alts) =>
        val modulo = (value / categorySteps.nn.stepSize).toInt
        alts(modulo.min(alts.length - 1))
      case Optional(feature) =>
        if value < 0.5f then None
        else
          Some(
            feature.fromNormalisedFloat(
              NormalisedFloat.applyUnsafe(2 * (value - 0.5f))
            )
          )

end Feature
