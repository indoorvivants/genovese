package genovese

import scala.util.boundary

class Train[T](
    featureful: Featureful[T],
    config: TrainingConfig,
    fitness: Fitness[T],
    events: EventHandler = EventHandler.None,
    evaluator: Evaluator = SequentialEvaluator
)(using RuntimeChecks):
  import config.*
  private val VECTOR_SIZE = featureful.features.length

  def train(): Map[T, NormalisedFloat] =

    var evaluated: Evaluated | Null = null

    boundary[Lab["training"]]:
      import TrainingEvent.*

      events.emit(TrainingStarted)

      var population = seed(populationSize)

      for step <- 0 until steps do
        events.emitWithData(EpochStarted, step)

        evaluated = evaluate(population)

        events.emitWithData(ReportFitness, fitnessStats(evaluated.nn))
        events.emitWithData(TopSpecimen, evaluated.nn.maxBy(_._2)._1)

        val selected = random.shuffle(select(evaluated.nn))

        val newPopulationBuilder = IArray.newBuilder[Vec]

        newPopulationBuilder.addAll(selected)

        boundary[Lab["crossover"]]:
          selected
            .grouped(2)
            .foreach:
              case a =>
                runtimeChecks.check(
                  Option.when(newPopulationBuilder.knownSize == -1)(
                    "known size of population builder cannot be -1, we rely on it"
                  )
                )

                if newPopulationBuilder.knownSize >= populationSize then
                  boundary.break(Lab("crossover"))

                crossover(a(0), a(1)).foreach(newPopulationBuilder.addOne)

        runtimeChecks.intBoundsCheck(0, selected.size, population.size)

        val newPop = newPopulationBuilder.result()
        if newPop.size < populationSize then
          population = Population(seed(populationSize - newPop.size) ++ newPop)
        else population = Population(newPop.take(populationSize))

        runtimeChecks.check(
          Option.when(population.size != populationSize)(
            s"Population size at step $step is [${population.size}]," +
              s" when [$populationSize] is expected"
          )
        )

        events.emitWithData(EpochFinished, step)
      end for

      events.emit(TrainingFinished, ignore = true)

    evaluated.nn.toSeq
      .map:
        case (vec, score) => materialize(vec) -> score
      .toMap
  end train

  private def fitnessStats(evaluated: Evaluated): FitnessStats =
    var min = NormalisedFloat.ONE
    var max = NormalisedFloat.ZERO
    var avg = 0.0f

    evaluated.foreach: (_, fitness) =>
      if fitness < min then min = fitness
      if fitness > max then max = fitness
      avg += fitness

    FitnessStats(
      max = max,
      min = min,
      avg = NormalisedFloat(avg / evaluated.length)
    )
  end fitnessStats

  private inline def materialize(vec: Vec): T =
    featureful.fromFeatures(vec)

  private def crossover(p1: Vec, p2: Vec): Array[Vec] =
    val crossoverPoint = random.nextInt(VECTOR_SIZE).max(1)
    val child1         = Vec(p1.take(crossoverPoint) ++ p2.drop(crossoverPoint))
    val child2         = Vec(p2.take(crossoverPoint) ++ p1.drop(crossoverPoint))

    Array(mutate(child1), mutate(child2))

  private def seed(n: Int): Population =
    Population(
      IArray.fill(n)(
        Vec(IArray.fill(VECTOR_SIZE)(randomNormalisedFloat))
      )
    )

  private inline def evaluate(pop: Population): Evaluated =
    evaluator.evaluate(pop, v => fitness(materialize(v)))
    // import scala.collection.parallel.CollectionConverters.*
    // Evaluated(
    //   IArray.unsafeFromArray(
    //     IArray
    //       .genericWrapArray(pop)
    //       .toArray
    //       .par
    //       .map(vec => vec -> fitness(materialize(vec)))
    //       .toArray
    //   )
    // )

  private def mutate(vec: Vec): Vec =
    if random.nextFloat() >= mutationRate then
      val idx = random.nextInt(VECTOR_SIZE)
      Vec(
        vec.updated(
          idx,
          NormalisedFloat(random.nextFloat())
        )
      )
    else vec

  private def select(pop: Evaluated): Population =
    selection match
      case Selection.Top(portion) =>
        Population(
          pop
            .sortBy(_._2)
            .takeRight((pop.size * portion).toInt)
            .map(_._1)
        )

  private inline def randomNormalisedFloat =
    NormalisedFloat(random.nextFloat())
end Train

extension (events: EventHandler)
  private[genovese] def emit(
      event: TrainingEvent[Nothing],
      ignore: Boolean = false
  )(using
      boundary.Label[Lab["training"]]
  ): Unit =
    if events.allowed.contains(event) then
      events.handle(event, null) match
        case TrainingInstruction.Halt if !ignore =>
          emit(TrainingEvent.TrainingInterrupted, ignore = true)
          boundary.break(Lab("training"))
        case _ =>

  private[genovese] def emitWithData[T](
      event: TrainingEvent[T],
      data: => T,
      ignore: Boolean = false
  )(using
      boundary.Label[Lab["training"]]
  ): Unit =
    if events.allowed.contains(event) then
      events.handle(event, data) match
        case TrainingInstruction.Halt if !ignore =>
          emit(TrainingEvent.TrainingInterrupted, ignore = true)
          boundary.break(Lab("training"))
        case _ =>
end extension
