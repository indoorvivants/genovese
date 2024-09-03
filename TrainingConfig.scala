package genovese

import scala.util.Random

case class TrainingConfig(
    populationSize: Int,
    mutationRate: NormalisedFloat,
    steps: Int,
    random: Random,
    selection: Selection
)
