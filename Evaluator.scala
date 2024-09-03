package genovese

trait Evaluator:
  def evaluate(population: Population, fitness: Vec => NormalisedFloat): Evaluated

object Evaluator extends EvaluatorCompanion

object SequentialEvaluator extends Evaluator:
  override def evaluate(population: Population, fitness: Vec => NormalisedFloat): Evaluated = 
    Evaluated(population.map(v => v -> fitness(v)))
