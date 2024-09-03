package genovese

import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.boundary
import scala.annotation.nowarn

final case class OptionFeatureful[T](original: Featureful[T])
    extends Featureful[Option[T]]:
  override lazy val features: IArray[Feature[?]] =
    original.features.map(Feature.Optional(_))
  override def fromFeatures(fv: IArray[NormalisedFloat]): Option[T] =
    boundary:
      val decompressed = fv.map: v =>
        if v < 0.5f then boundary.break(None)
        else NormalisedFloat.applyUnsafe(2 * (v - 0.5f))
      Some(original.fromFeatures(decompressed))

  override def toFeatures(value: Option[T])(using
      RuntimeChecks
  ): IArray[NormalisedFloat] =
    value match
      case None =>
        IArray.fill(original.features.size)(NormalisedFloat.applyUnsafe(0.25f))
      case Some(value) =>
        original
          .toFeatures(value)
          .map(v => NormalisedFloat.applyUnsafe(v / 2 + 0.5f))
end OptionFeatureful

