package genovese

opaque type Fitness[T] <: T => NormalisedFloat = T => NormalisedFloat

object Fitness:
  inline def apply[T](f: T => NormalisedFloat): Fitness[T] = f
