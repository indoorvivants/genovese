package genovese

object ParallelCollectionsEvaluator extends Evaluator:
  override def evaluate(pop: Population, fitness: Vec => NormalisedFloat): Evaluated = 
    import scala.collection.parallel.CollectionConverters.*
    Evaluated(
      IArray.unsafeFromArray(
        IArray
          .genericWrapArray(pop)
          .toArray
          .par
          .map(vec => vec -> fitness(vec))
          .toArray
      )
    )

private[genovese] trait EvaluatorCompanion:
  val default = ParallelCollectionsEvaluator
