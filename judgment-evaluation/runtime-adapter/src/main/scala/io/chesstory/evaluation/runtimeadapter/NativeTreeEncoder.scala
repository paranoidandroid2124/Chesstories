package io.chesstory.evaluation.runtimeadapter

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.{ HexFormat, IdentityHashMap }

import scala.collection.{ Iterable, Map, Seq, Set }

import play.api.libs.json.*

/** A deterministic, value-oriented snapshot of the runtime's immutable native ADTs.
  *
  * This encoder intentionally has no chess or judgment-specific cases. It reads
  * case-class/enum products through Product's public API and rejects values outside
  * the explicitly supported structural universe.
  */
private[runtimeadapter] object NativeTreeEncoder:
  val ContractVersion = "chesstory.runtime-native-tree.v2"
  val HashContract = "chesstory.sorted-object-keys-json-sha256.v1"

  final class EncodingFailure private[runtimeadapter] (
      val code: String,
      val nativeType: Option[String]
  ) extends RuntimeException(code)

  final case class StructuralProductView(
      scalaType: String,
      constructor: String,
      fields: List[(String, Any)]
  )

  trait StructuralAdapter:
    def adapt(value: Any): Option[StructuralProductView]

  def encode(value: Any, adapters: List[StructuralAdapter] = Nil): JsObject =
    EncoderState(adapters).encode(value, path = "$", depth = 0)

  def canonicalString(value: JsValue): String =
    value match
      case JsNull          => "null"
      case JsBoolean(item) => if item then "true" else "false"
      case JsString(item)  => Json.stringify(JsString(item))
      case JsNumber(item)  => item.bigDecimal.toString
      case JsArray(items)  => items.iterator.map(canonicalString).mkString("[", ",", "]")
      case JsObject(fields) =>
        fields.toList
          .sortBy(_._1)
          .iterator
          .map { case (key, item) =>
            s"${Json.stringify(JsString(key))}:${canonicalString(item)}"
          }
          .mkString("{", ",", "}")

  def sha256Canonical(value: JsValue): String =
    sha256(canonicalString(value).getBytes(StandardCharsets.UTF_8))

  def sha256Utf8(value: String): String =
    sha256(value.getBytes(StandardCharsets.UTF_8))

  private def sha256(bytes: Array[Byte]): String =
    HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes))

  private final class EncoderState(adapters: List[StructuralAdapter]):
    private val active = IdentityHashMap[AnyRef, java.lang.Boolean]()
    private var nodeCount = 0

    def encode(value: Any, path: String, depth: Int): JsObject =
      nodeCount += 1
      if depth > MaxDepth then fail("maximum_depth_exceeded", value)
      if nodeCount > MaxNodes then fail("maximum_node_count_exceeded", value)

      val adapted = adapters.iterator.flatMap(_.adapt(value)).nextOption
      adapted match
        case Some(view) => encodeStructuralView(value, view, path, depth)
        case None       => encodeSupported(value, path, depth)

    private def encodeSupported(value: Any, path: String, depth: Int): JsObject =
      value match
        case null =>
          Json.obj("node_kind" -> "null")
        case item: Boolean =>
          Json.obj(
            "node_kind" -> "boolean",
            "scala_type" -> "scala.Boolean",
            "value" -> item
          )
        case item: String =>
          Json.obj(
            "node_kind" -> "string",
            "scala_type" -> "java.lang.String",
            "value" -> item
          )
        case item: Char =>
          Json.obj(
            "node_kind" -> "char",
            "scala_type" -> "scala.Char",
            "code_point" -> item.toInt,
            "value" -> item.toString
          )
        case item: Byte => integral("scala.Byte", item.toString)
        case item: Short => integral("scala.Short", item.toString)
        case item: Int => integral("scala.Int", item.toString)
        case item: Long => integral("scala.Long", item.toString)
        case item: Float => floating(
            scalaType = "scala.Float",
            value = java.lang.Float.toHexString(item),
            rawBits = f"${java.lang.Float.floatToRawIntBits(item)}%08x"
          )
        case item: Double => floating(
            scalaType = "scala.Double",
            value = java.lang.Double.toHexString(item),
            rawBits = f"${java.lang.Double.doubleToRawLongBits(item)}%016x"
          )
        case item: BigInt => integral("scala.math.BigInt", item.toString)
        case item: BigDecimal => decimal(
            scalaType = "scala.math.BigDecimal",
            value = item.bigDecimal
          )
        case item: java.math.BigInteger => integral("java.math.BigInteger", item.toString)
        case item: java.math.BigDecimal => decimal(
            scalaType = "java.math.BigDecimal",
            value = item
          )
        case item: JsValue =>
          encodePlayJson(item, path, depth)
        case item: Option[?] =>
          withComposite(item.asInstanceOf[AnyRef], path) {
            item match
              case Some(inner) =>
                Json.obj(
                  "node_kind" -> "option",
                  "scala_type" -> "scala.Option",
                  "variant" -> "Some",
                  "value" -> encode(inner, s"$path.value", depth + 1)
                )
              case None =>
                Json.obj(
                  "node_kind" -> "option",
                  "scala_type" -> "scala.Option",
                  "variant" -> "None"
                )
          }
        case item: Either[?, ?] =>
          withComposite(item.asInstanceOf[AnyRef], path) {
            item match
              case Left(inner) =>
                Json.obj(
                  "node_kind" -> "either",
                  "scala_type" -> "scala.Either",
                  "variant" -> "Left",
                  "value" -> encode(inner, s"$path.value", depth + 1)
                )
              case Right(inner) =>
                Json.obj(
                  "node_kind" -> "either",
                  "scala_type" -> "scala.Either",
                  "variant" -> "Right",
                  "value" -> encode(inner, s"$path.value", depth + 1)
                )
          }
        case item: Array[Byte] => encodeArray(item, item.iterator, path, depth)
        case item: Array[Short] => encodeArray(item, item.iterator, path, depth)
        case item: Array[Int] => encodeArray(item, item.iterator, path, depth)
        case item: Array[Long] => encodeArray(item, item.iterator, path, depth)
        case item: Array[Float] => encodeArray(item, item.iterator, path, depth)
        case item: Array[Double] => encodeArray(item, item.iterator, path, depth)
        case item: Array[Boolean] => encodeArray(item, item.iterator, path, depth)
        case item: Array[Char] => encodeArray(item, item.iterator, path, depth)
        case item: Array[?] => encodeArray(item, item.iterator, path, depth)
        case item: Map[?, ?] =>
          withComposite(item.asInstanceOf[AnyRef], path) {
            val entries = item.iterator.zipWithIndex.map { case ((key, mapValue), index) =>
              Json.obj(
                "key" -> encode(key, s"$path.entries[$index].key", depth + 1),
                "value" -> encode(mapValue, s"$path.entries[$index].value", depth + 1)
              )
            }.toList.sortBy(canonicalString)
            Json.obj(
              "node_kind" -> "map",
              "collection_type" -> runtimeType(item),
              "entries" -> entries
            )
          }
        case item: Set[?] =>
          withComposite(item.asInstanceOf[AnyRef], path) {
            val values = item.iterator.zipWithIndex
              .map { case (inner, index) => encode(inner, s"$path.values[$index]", depth + 1) }
              .toList
              .sortBy(canonicalString)
            Json.obj(
              "node_kind" -> "set",
              "collection_type" -> runtimeType(item),
              "values" -> values
            )
          }
        case item: Seq[?] =>
          encodeOrderedCollection("sequence", item, item.iterator, path, depth)
        case item: Iterable[?] =>
          encodeOrderedCollection("iterable", item, item.iterator, path, depth)
        case item: java.lang.Enum[?] =>
          Json.obj(
            "node_kind" -> "java-enum",
            "scala_type" -> runtimeType(item),
            "constant" -> item.name()
          )
        case item: Product =>
          withComposite(item.asInstanceOf[AnyRef], path) {
            val names = item.productElementNames.toList
            val values = item.productIterator.toList
            if names.size != item.productArity || values.size != item.productArity then
              fail("product_shape_mismatch", item)
            val fields = names.zip(values).zipWithIndex.map { case ((name, inner), index) =>
              Json.obj(
                "index" -> index,
                "name" -> name,
                "value" -> encode(inner, s"$path.$name", depth + 1)
              )
            }
            Json.obj(
              "node_kind" -> "product",
              "scala_type" -> runtimeType(item),
              "constructor" -> item.productPrefix,
              "arity" -> item.productArity,
              "fields" -> fields
            )
          }
        case item =>
          fail("unsupported_native_type", item)

    private def encodeStructuralView(
        original: Any,
        view: StructuralProductView,
        path: String,
        depth: Int
    ): JsObject =
      val reference = Option(original).map(_.asInstanceOf[AnyRef]).getOrElse(
        fail("null_structural_adapter_target", original)
      )
      if view.scalaType.isEmpty || view.constructor.isEmpty then
        fail("invalid_structural_adapter_metadata", original)
      withComposite(reference, path) {
        val fields = view.fields.zipWithIndex.map { case ((name, inner), index) =>
          Json.obj(
            "index" -> index,
            "name" -> name,
            "value" -> encode(inner, s"$path.$name", depth + 1)
          )
        }
        Json.obj(
          "node_kind" -> "product",
          "scala_type" -> view.scalaType,
          "constructor" -> view.constructor,
          "arity" -> view.fields.size,
          "fields" -> fields
        )
      }

    private def encodeArray(
        array: AnyRef,
        iterator: Iterator[?],
        path: String,
        depth: Int
    ): JsObject =
      withComposite(array, path) {
        Json.obj(
          "node_kind" -> "array",
          "collection_type" -> runtimeType(array),
          "values" -> iterator.zipWithIndex.map { case (inner, index) =>
            encode(inner, s"$path.values[$index]", depth + 1)
          }.toList
        )
      }

    private def encodeOrderedCollection(
        nodeKind: String,
        collection: AnyRef,
        iterator: Iterator[?],
        path: String,
        depth: Int
    ): JsObject =
      withComposite(collection, path) {
        Json.obj(
          "node_kind" -> nodeKind,
          "collection_type" -> runtimeType(collection),
          "values" -> iterator.zipWithIndex.map { case (inner, index) =>
            encode(inner, s"$path.values[$index]", depth + 1)
          }.toList
        )
      }

    private def encodePlayJson(value: JsValue, path: String, depth: Int): JsObject =
      value match
        case JsNull =>
          Json.obj(
            "node_kind" -> "play-json",
            "json_node_type" -> "null"
          )
        case JsBoolean(item) =>
          Json.obj(
            "node_kind" -> "play-json",
            "json_node_type" -> "boolean",
            "value" -> item
          )
        case JsString(item) =>
          Json.obj(
            "node_kind" -> "play-json",
            "json_node_type" -> "string",
            "value" -> item
          )
        case JsNumber(item) =>
          val decimalValue = item.bigDecimal
          Json.obj(
            "node_kind" -> "play-json",
            "json_node_type" -> "number",
            "value" -> decimalValue.toString,
            "unscaled_value" -> decimalValue.unscaledValue.toString,
            "scale" -> decimalValue.scale,
            "precision" -> decimalValue.precision
          )
        case item: JsArray =>
          withComposite(item, path) {
            Json.obj(
              "node_kind" -> "play-json",
              "json_node_type" -> "array",
              "values" -> item.value.zipWithIndex.map { case (inner, index) =>
                encodePlayJson(inner, s"$path.values[$index]", depth + 1)
              }
            )
          }
        case item: JsObject =>
          withComposite(item, path) {
            val fields = item.fields.toList.sortBy(_._1).map { case (name, inner) =>
              Json.obj(
                "name" -> name,
                "value" -> encodePlayJson(inner, s"$path.$name", depth + 1)
              )
            }
            Json.obj(
              "node_kind" -> "play-json",
              "json_node_type" -> "object",
              "fields" -> fields
            )
          }

    private def integral(scalaType: String, value: String): JsObject =
      Json.obj(
        "node_kind" -> "number",
        "scala_type" -> scalaType,
        "encoding" -> "base10",
        "value" -> value
      )

    private def floating(scalaType: String, value: String, rawBits: String): JsObject =
      Json.obj(
        "node_kind" -> "number",
        "scala_type" -> scalaType,
        "encoding" -> "ieee754-hex",
        "value" -> value,
        "raw_bits" -> rawBits
      )

    private def decimal(scalaType: String, value: java.math.BigDecimal): JsObject =
      Json.obj(
        "node_kind" -> "number",
        "scala_type" -> scalaType,
        "encoding" -> "unscaled-base10-with-scale",
        "value" -> value.toString,
        "unscaled_value" -> value.unscaledValue.toString,
        "scale" -> value.scale,
        "precision" -> value.precision
      )

    private def withComposite[A <: JsObject](reference: AnyRef, path: String)(body: => A): A =
      if active.containsKey(reference) then
        throw new EncodingFailure("cycle_detected", Some(runtimeType(reference)))
      active.put(reference, java.lang.Boolean.TRUE)
      try body
      finally active.remove(reference)

    private def fail(code: String, value: Any): Nothing =
      throw new EncodingFailure(code, Option(value).map(runtimeType))

  private object EncoderState:
    def apply(adapters: List[StructuralAdapter]): EncoderState = new EncoderState(adapters)

  private def runtimeType(value: Any): String =
    value.asInstanceOf[AnyRef].getClass.getName

  private val MaxDepth = 512
  private val MaxNodes = 2_000_000
