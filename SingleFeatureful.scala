package genovese

final case class SingleFeatureful[T](feature: Feature[T]) extends Featureful[T]:
  override val features: IArray[Feature[?]] = IArray(feature)

  override def toFeatures(value: T)(using
      RuntimeChecks
  ): IArray[NormalisedFloat] =
    IArray(feature.toNormalisedFloat(value))

  override def fromFeatures(fv: IArray[NormalisedFloat]): T =
    feature.fromNormalisedFloat(fv(0).asInstanceOf[NormalisedFloat])
end SingleFeatureful
