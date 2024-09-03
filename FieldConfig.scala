package genovese

case class FieldConfig(overrides: Map[String, Feature[?]])

object FieldConfig:
  inline def None = FieldConfig(Map.empty)
