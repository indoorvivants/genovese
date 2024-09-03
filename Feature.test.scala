package genovese

import munit.*
import org.scalacheck.Prop.*
import org.scalacheck.Gen

class FeatureTest extends FunSuite, ScalaCheckSuite:
  import NormalisedFloat.*
  given RuntimeChecks = RuntimeChecks.Full

  val genNormalisedFloat = Gen.chooseNum(0.0f, 1.0f).map(NormalisedFloat(_))

  object gen:
    val bool            = Gen.const(Feature.Bool)
    val normalisedFloat = Gen.const(Feature.NormalisedFloatRange)

    val stringCategory: Gen[Feature.StringCategory] =
      for
        n <- Gen.chooseNum(1, 10)
        alts = List.tabulate(n)(i => s"alt$i")
      yield Feature.StringCategory(alts)

    val intCategory: Gen[Feature.IntCategory] =
      for
        n    <- Gen.chooseNum(1, 10)
        alts <- Gen.listOfN(n, Gen.posNum[Int])
      yield Feature.IntCategory(alts)

    val feature = Gen.oneOf(bool, stringCategory, intCategory, normalisedFloat)
  end gen

  test("Bool: toNormalisedFloat basics"):
    val bool = Feature.Bool

    assertEquals(bool.toNormalisedFloat(true), 1.0f)
    assertEquals(bool.toNormalisedFloat(false), 0.0f)

  property("Bool: fromNormalisedFloat base cases"):
    val bool = Feature.Bool

    assertEquals(bool.fromNormalisedFloat(ZERO), false)
    assertEquals(bool.fromNormalisedFloat(ONE), true)

  property("Bool: fromNormalisedFloat range"):
    val bool = Feature.Bool

    val g = Gen.chooseNum(0.0f, 1.0f).map(NormalisedFloat.apply)

    forAll(g): x =>
      bool.fromNormalisedFloat(x) == (x >= 0.5f)

  property("IntCategory"):
    val g =
      for
        n      <- Gen.chooseNum(1, 10)
        flt    <- genNormalisedFloat
        alts   <- Gen.listOfN(n, Gen.posNum[Int])
        chosen <- Gen.chooseNum(0, n - 1).map(alts.apply)
      yield (Feature.IntCategory(alts), alts, chosen, flt)

    forAll(g): (feature, alts, chosen, float) =>
      feature.fromNormalisedFloat(
        feature.toNormalisedFloat(chosen)
      ) == chosen
        &&
        alts.contains(feature.fromNormalisedFloat(float))

  property("Optional"):
    forAll(gen.feature, genNormalisedFloat): (feat, float) =>
      val o = Feature.Optional(feat)
      ((float <= 0.5f) && o.fromNormalisedFloat(float) == None) ||
      (float > 0.5f) && o.fromNormalisedFloat(float).isDefined

  property("StringCategory"):
    val g =
      for
        n   <- Gen.chooseNum(1, 10)
        flt <- genNormalisedFloat
        alts = List.tabulate(n)(i => s"alt$i")
        chosen <- Gen.chooseNum(0, n - 1).map(alts.apply)
      yield (Feature.StringCategory(alts), alts, chosen, flt)

    forAll(g): (feature, alts, chosen, float) =>
      feature.fromNormalisedFloat(
        feature.toNormalisedFloat(chosen)
      ) == chosen
        &&
        alts.contains(feature.fromNormalisedFloat(float))

  property("FloatRange: normalisation roundtrip"):
    val gen = for
      min <- Gen.double.map(_.toFloat)
      len <- Gen.posNum[Float]
      num <- Gen.chooseNum(min, min + len)
    yield (Feature.FloatRange(min, min + len), num)

    forAll(gen): (float, num) =>
      Math.abs(
        float.fromNormalisedFloat(float.toNormalisedFloat(num)) - num
      ) <= 0.0001

end FeatureTest
