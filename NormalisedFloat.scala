package genovese

opaque type NormalisedFloat <: Float = Float

object NormalisedFloat:
  inline def apply(value: Float)(using
      rc: RuntimeChecks = RuntimeChecks.Full
  ): NormalisedFloat =
    runtimeChecks.floatBoundsCheck(0.0f, value, 1.0f)
    value

  inline def applyUnsafe(value: Float): NormalisedFloat =
    value

  inline def ZERO: NormalisedFloat = 0.0f
  inline def ONE: NormalisedFloat  = 1.0f
end NormalisedFloat
