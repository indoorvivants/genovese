package genovese

import munit.FunSuite

class TrainTest extends FunSuite:

  test("Training"):
    case class Breaks(one: Boolean, two: Boolean, width: Float)

    given Featureful[Breaks] =
      Featureful.derive(
        FieldConfig(Map("width" -> Feature.FloatRange(50, 150)))
      )

    case class FormattingConfig(
        format: Boolean,
        scala3: Boolean,
        lineLength: Float,
        breaks: Breaks
    )

    given Featureful[FormattingConfig] =
      Featureful.derive(
        FieldConfig(Map("lineLength" -> Feature.FloatRange(50, 140)))
      )

    given RuntimeChecks = RuntimeChecks.Full

    val trainingConfig = TrainingConfig(
      populationSize = 100,
      mutationRate = NormalisedFloat(0.5f),
      steps = 100,
      random = scala.util.Random(80085L),
      selection = Selection.Top(0.8)
    )

    val default = FormattingConfig(true, true, 0, Breaks(false, false, 0.0f))

    val fitness = Fitness[FormattingConfig]: c =>
      var score = 0.0f
      val step  = 0.1f

      if c.format then score += step
      if c.scala3 then score += step
      if c.lineLength > 110 && c.lineLength < 112 then score += step
      if c.breaks.width > 81 && c.breaks.width < 83f then score += step
      if c.breaks.one then score += step
      if !c.breaks.two then score += step

      NormalisedFloat(score)

    object Handler extends EventHandler:
      import TrainingEvent.*, TrainingInstruction.*
      def handle[T](t: TrainingEvent[T], data: T | Null): TrainingInstruction =
        t match
          case r @ ReportFitness =>
            if ReportFitness.cast(data).max >= 0.6f then Halt
            else Continue
          case _ => Continue
      end handle

      override val allowed: Set[TrainingEvent[?]] =
        all -- Set(
          TopSpecimen,
          EpochFinished,
          EpochStarted
        )
    end Handler

    val top = Train(
      summon[Featureful[FormattingConfig]],
      config = trainingConfig,
      fitness = fitness,
      events = Handler,
    ).train().maxBy(_._2)._1

    assertEquals(fitness(top), 0.6f)

end TrainTest
