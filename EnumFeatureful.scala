package genovese

import scala.reflect.ClassTag

final case class EnumFeatureful[T](values: IArray[T]) extends Featureful[T]:
  private val lookup = values.zipWithIndex.toMap
  override lazy val features: IArray[Feature[?]] = IArray(
    Feature.IntCategory(List.tabulate(values.length)(identity))
  )
  override def toFeatures(value: T)(using
      RuntimeChecks
  ): IArray[NormalisedFloat] =
    features.map:
      case f @ Feature.IntCategory(_) =>
        f.toNormalisedFloat(lookup(value))

  override def fromFeatures(
      fv: IArray[NormalisedFloat]
  ): T =
    features(0): @unchecked match
      case f @ Feature.IntCategory(_) =>
        val idx =
          f.fromNormalisedFloat(NormalisedFloat.applyUnsafe(fv(0)))
        values(idx)

  override def toString(): String = s"EnumFeatureful(${values.mkString(",")})"
end EnumFeatureful

