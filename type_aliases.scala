package genovese

import opaque_newtypes.*

opaque type Vec <: IArray[NormalisedFloat] = IArray[NormalisedFloat]
object Vec extends SubtypeWrapper[Vec, IArray[NormalisedFloat]]

opaque type Population <: IArray[Vec] = IArray[Vec]
object Population extends SubtypeWrapper[Population, IArray[Vec]]

opaque type Evaluated <: IArray[(Vec, NormalisedFloat)] =
  IArray[(Vec, NormalisedFloat)]
object Evaluated extends SubtypeWrapper[Evaluated, IArray[(Vec, NormalisedFloat)]]


trait SubtypeWrapper[Newtype, Impl](using ev: Newtype =:= Impl):
  inline def raw(inline a: Newtype): Impl   = ev.apply(a)
  inline def apply(inline s: Impl): Newtype = ev.flip.apply(s)

  private val flipped = ev.flip

  given SameRuntimeType[Newtype, Impl] = new:
    override def apply(a: Newtype): Impl = ev.apply(a)

  given SameRuntimeType[Impl, Newtype] = new:
    override def apply(a: Impl): Newtype = flipped.apply(a)

