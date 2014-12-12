package argonaut.internal

import argonaut._
import scala.collection.immutable.{ SortedMap, MapLike }
import scalaz._, Scalaz._

object Macros extends MacrosCompat {
  def materializeCodecImpl[T: c.WeakTypeTag](c: Context): c.Expr[CodecJson[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val encode = materializeEncodeImpl[T](c)
    val decode = materializeDecodeImpl[T](c)
    c.Expr[CodecJson[T]](q"""
    CodecJson.derived[$tpe]($encode, $decode)
    """)
  }

  def materializeEncodeImpl[T: c.WeakTypeTag](c: Context): c.Expr[EncodeJson[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val primaryConstructor = tpe.decls.collectFirst{
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }
    primaryConstructor match {
      case Some(constructor) => {
        val fieldNames: List[c.universe.Name] = constructor.paramLists.flatten.map{field => 
          field.name
        }
        val decodedNames: List[String] = fieldNames.map(_.decodedName.toString)
        val fieldTypes: List[c.universe.Type] = constructor.paramLists.flatten.map{field =>
          tpe.decl(field.name).typeSignature
        }
        val fieldCount = fieldNames.size
        val invocations = decodedNames.map{fieldName => 
          val termName = TermName(fieldName)
          q"toEncode.$termName"
        }
        val methodName = TermName("jencode" + (fieldCount.toString) + "L")
        c.Expr[EncodeJson[T]]{q"""
          EncodeJson.$methodName[$tpe, ..$fieldTypes](toEncode => (..$invocations))(..$decodedNames)
        """}
      }
      case None => c.abort(c.enclosingPosition, "Could not identify primary constructor for " + tpe)
    }
  }

  def materializeDecodeImpl[T: c.WeakTypeTag](c: Context): c.Expr[DecodeJson[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val primaryConstructor = tpe.decls.collectFirst{
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }
    primaryConstructor match {
      case Some(constructor) => {
        val fieldNames: List[c.universe.Name] = constructor.paramLists.flatten.map{field => 
          field.name
        }
        val decodedNames: List[String] = fieldNames.map(_.decodedName.toString)
        val fieldTypes: List[c.universe.Type] = constructor.paramLists.flatten.map{field =>
          tpe.decl(field.name).typeSignature
        }
        val fieldCount = fieldNames.size
        val functionParameters = decodedNames.zip(fieldTypes).map{case (fieldName, fieldType) =>
          val termName = TermName(fieldName)
          q"$termName: $fieldType"
        }
        val parameters = decodedNames.map{fieldName =>
          val termName = TermName(fieldName)
          q"$termName"
        }
        val methodName = TermName("jdecode" + (fieldCount.toString) + "L")
        c.Expr[DecodeJson[T]]{q"""
          DecodeJson.$methodName[..$fieldTypes, $tpe]((..$functionParameters) => new $tpe(..$parameters))(..$decodedNames)
        """}
      }
      case None => c.abort(c.enclosingPosition, "Could not identify primary constructor for " + tpe)
    }
  }
}