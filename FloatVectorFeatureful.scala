package genovese

case class FloatVectorFeatureful(len: Int)
    extends Featureful[IArray[NormalisedFloat]]:
  override val features: IArray[Feature[?]] =
    IArray.fill(len)(Feature.NormalisedFloatRange)

  override def fromFeatures(
      fv: IArray[NormalisedFloat]
  ): IArray[NormalisedFloat] =
    fv

  override def toFeatures(
      value: IArray[NormalisedFloat]
  )(using RuntimeChecks): IArray[NormalisedFloat] = value
end FloatVectorFeatureful
