package genovese

case class FitnessStats(
    max: NormalisedFloat,
    min: NormalisedFloat,
    avg: NormalisedFloat
):
  override def toString(): String =
    s"FitnessStats(max=$max, min=$min, avg=$avg)"
