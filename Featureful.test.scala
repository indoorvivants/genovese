package genovese

import munit.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import Featureful.*

class FeaturefulTest extends FunSuite, ScalaCheckSuite:
  given RuntimeChecks = RuntimeChecks.Full

  case class Breaks(x: Boolean, y: Boolean) derives Featureful
  case class Cfg(test: Boolean, breaks: Breaks, z: Boolean) derives Featureful

  sealed abstract class Oneline derives Featureful
  object Oneline:
    case object keep   extends Oneline
    case object fold   extends Oneline
    case object unfold extends Oneline

  enum Wrap derives Featureful:
    case no, standalone, trailing

  case class Config(
      test: Boolean,
      h: Float,
      x: Int,
      z: Float,
      d: NormalisedFloat,
      r: String,
      f: Oneline,
      t: Option[Boolean] = None,
      t2: Option[String] = None
  )

  given Featureful[Config] =
    derive[Config](
      FieldConfig(
        Map(
          "h"  -> Feature.FloatRange(1, 25),
          "x"  -> Feature.IntRange(500, 1500),
          "z"  -> Feature.FloatRange(300f, 600f),
          "r"  -> Feature.StringCategory(List("a", "b")),
          "t2" -> Feature.StringCategory(List("hello", "world"))
        )
      )
    )

  case class TestEnums(
      e1: Oneline,
      w1: Wrap
  ) derives Featureful

  test("roundtrip and features"):

    assertEquals(
      features[Config].toList,
      List(
        Feature.Bool,
        Feature.FloatRange(1.0f, 25.0f),
        Feature.IntRange(500, 1500),
        Feature.FloatRange(300f, 600f),
        Feature.NormalisedFloatRange,
        Feature.StringCategory(List("a", "b")),
        Feature.IntCategory(List(0, 1, 2)),
        Feature.Optional(Feature.Bool),
        Feature.Optional(Feature.StringCategory(List("hello", "world")))
      )
    )

    val example =
      Config(
        true,
        15f,
        800,
        450f,
        NormalisedFloat(0.6f),
        "a",
        Oneline.keep,
        Some(false),
        Some("hello")
      )

    assertEquals(roundtrip(example), example)

    assertEquals(
      toFeatures(Breaks(true, false)).toSeq,
      IArray(1.0f, 0.0f).toSeq
    )

    assertEquals(
      toFeatures(Breaks(false, false)).toSeq,
      IArray(0.0f, 0.0f).toSeq
    )

    assertEquals(
      toFeatures(Cfg(true, Breaks(false, true), false)).toSeq,
      IArray(1.0f, 0.0f, 1.0f, 0.0f).toSeq
    )

  property("enums roundtrip"):
    forAll(
      Gen.oneOf(Wrap.values.toSeq),
      Gen.oneOf(Oneline.keep, Oneline.fold, Oneline.unfold)
    ): (wrap, oneline) =>
      val value = TestEnums(oneline, wrap)
      roundtrip(value) == value

  property("tuples roundtrip"):
    given f: Featureful[(Oneline, Wrap)] = deriveTuple[(Oneline, Wrap)]

    forAll(
      Gen.oneOf(Wrap.values.toSeq),
      Gen.oneOf(Oneline.keep, Oneline.fold, Oneline.unfold)
    ): (wrap, oneline) =>
      roundtrip(oneline -> wrap) == oneline -> wrap

  def roundtrip[T: Featureful](value: T): T =
    fromFeatures[T](toFeatures(value))

end FeaturefulTest
