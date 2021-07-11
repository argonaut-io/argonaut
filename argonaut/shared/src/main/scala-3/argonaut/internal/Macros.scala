package argonaut
package internal

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonFrom}

object Macros {
  inline def summonLabels[T <: Tuple]: Array[String] =
    summonLabelsRec[T].toArray

  inline def summonDecoders[T <: Tuple]: Array[DecodeJson[_]] =
    summonDecodersRec[T].toArray

  inline def summonEncoders[T <: Tuple]: Array[EncodeJson[_]] =
    summonEncodersRec[T].toArray

  inline def summonEncoder[A]: EncodeJson[A] =
    summonFrom {
      case x: EncodeJson[A] =>
        x
      case _: Mirror.ProductOf[A] =>
        Macros.derivedEncoder[A]
    }

  inline def summonDecoder[A]: DecodeJson[A] =
    summonFrom {
      case x: DecodeJson[A] =>
        x
      case _: Mirror.ProductOf[A] =>
        Macros.derivedDecoder[A]
    }

  inline def summonCodec[A]: CodecJson[A] =
    summonFrom {
      case x: CodecJson[A] =>
        x
      case _: Mirror.ProductOf[A] =>
        Macros.derivedCodec[A]
    }

  inline def summonLabelsRec[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple =>
        Nil
      case _: (t *: ts) =>
        constValue[t].asInstanceOf[String] :: summonLabelsRec[ts]
    }

  inline def summonDecodersRec[T <: Tuple]: List[DecodeJson[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple =>
        Nil
      case _: (t *: ts) =>
        summonDecoder[t] :: summonDecodersRec[ts]
    }

  inline def summonEncodersRec[T <: Tuple]: List[EncodeJson[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple =>
        Nil
      case _: (t *: ts) =>
        summonEncoder[t] :: summonEncodersRec[ts]
    }

  inline def derivedEncoder[A](using inline A: Mirror.ProductOf[A]): EncodeJson[A] =
    new EncodeJson[A] {
      implicit def self: EncodeJson[A] = this // for recursive type

      private[this] val elemEncoders: Array[EncodeJson[_]] =
        Macros.summonEncoders[A.MirroredElemTypes]

      override def encode(a: A): Json =
        Json.jObject(
          createJsonObject(a.asInstanceOf[Product])
        )

      private[this] def createJsonObject(value: Product): JsonObject = {
        def encodeWith(index: Int)(p: Any): (String, Json) = {
          (value.productElementName(index), elemEncoders(index).asInstanceOf[EncodeJson[Any]].apply(p))
        }
        val elems: Iterator[Any] = value.productIterator
        @tailrec def loop(i: Int, acc: JsonObject): JsonObject = {
          if (elems.hasNext) {
            val field = encodeWith(i)(elems.next())
            loop(i + 1, acc :+ field)
          } else {
            acc
          }
        }
        loop(0, JsonObject.empty)
      }
    }

  inline def derivedCodec[A](using inline A: Mirror.ProductOf[A]): CodecJson[A] =
    CodecJson.derived[A](
      E = derivedEncoder[A],
      D = derivedDecoder[A],
    )

  inline def derivedDecoder[A](using inline A: Mirror.ProductOf[A]): DecodeJson[A] =
    new DecodeJson[A] {
      implicit def self: DecodeJson[A] = this // for recursive type

      private[this] def decodeWith(index: Int)(c: HCursor): DecodeResult[AnyRef] =
        elemDecoders(index).asInstanceOf[DecodeJson[AnyRef]].tryDecode(c.downField(elemLabels(index)))

      private[this] def resultIterator(c: HCursor): Iterator[DecodeResult[AnyRef]] =
        new AbstractIterator[DecodeResult[AnyRef]] {
          private[this] var i: Int = 0

          def hasNext: Boolean = i < elemCount

          def next: DecodeResult[AnyRef] = {
            val result = decodeWith(i)(c)
            i += 1
            result
          }
        }

      private[this] val elemLabels = Macros.summonLabels[A.MirroredElemLabels]

      private[this] val elemDecoders: Array[DecodeJson[_]] =
        Macros.summonDecoders[A.MirroredElemTypes]

      private[this] val elemCount = elemDecoders.size

      override def decode(c: HCursor): DecodeResult[A] = {
        DecodeResult[A] {
          val iter = resultIterator(c)
          val res = new Array[AnyRef](elemCount)
          var failed: (String, CursorHistory) = null
          var i: Int = 0

          while (iter.hasNext && (failed eq null)) {
            iter.next.result match {
              case Right(value) =>
                res(i) = value
              case Left(l) =>
                failed = l
            }
            i += 1
          }

          if (failed eq null) {
            Right(
              A.fromProduct(
                new Product{
                  override def canEqual(that: Any): Boolean =
                    true
                  override def productArity: Int =
                    res.length
                  override def productElement(n: Int): Any =
                    res.apply(n)
                }
              )
            )
          } else {
            Left(failed)
          }
        }
      }
    }
}
