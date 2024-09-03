package genovese

trait EventHandler:
  def handle[T](te: TrainingEvent[T], data: T | Null): TrainingInstruction
  val allowed: Set[TrainingEvent[?]] = TrainingEvent.all

object EventHandler:
  val None = new EventHandler:
    override def handle[T](
        te: TrainingEvent[T],
        data: T | Null
    ): TrainingInstruction = TrainingInstruction.Continue
    override val allowed: Set[TrainingEvent[?]] = Set.empty
