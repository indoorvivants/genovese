package genovese

enum TrainingEvent[+Data]:
  case TrainingStarted, TrainingInterrupted, TrainingFinished
  case EpochStarted  extends TrainingEvent[Int]
  case EpochFinished extends TrainingEvent[Int]
  case ReportFitness extends TrainingEvent[FitnessStats]
  case TopSpecimen   extends TrainingEvent[IArray[NormalisedFloat]]

  def cast(a: Any): Data = a.asInstanceOf[Data]

object TrainingEvent:
  val all = values.toSet
