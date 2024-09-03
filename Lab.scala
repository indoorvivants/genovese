package genovese

private[genovese] opaque type Lab[T <: Singleton] >: Unit = Unit

private[genovese] object Lab:
  inline def apply[T <: Singleton](inline str: T): Lab[T] = ()
