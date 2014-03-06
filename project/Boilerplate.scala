import sbt._

object Boilerplate {
  val arities = (1 to 22)
  val aritiesExceptOne = (2 to 22)
  val arityChars: Map[Int, String] = arities.map(n => (n, ('A' + n - 1).toChar.toString)).toMap

  def write(path: File, fileContents: String): File = {
    IO.write(path, fileContents)
    path
  }

  def gen(dir : File) = {
    val generatedDecodeJson = write(dir / "argonaut" / "GeneratedDecodeJsons.scala", genDecodeJsons)

    val generatedEncodeJson = write(dir / "argonaut" / "GeneratedEncodeJsons.scala", genEncodeJsons)

    val generatedCodecJson = write(dir / "argonaut" / "GeneratedCodecJsons.scala", genCodecJsons)

    Seq(generatedDecodeJson, generatedEncodeJson, generatedCodecJson)
  }

  def header = {
    """|
       |package argonaut
       |
       |""".stripMargin
  }

  def functionTypeParameters(arity: Int): String = (1 to arity).map(n => arityChars(n)).mkString(", ")

  def tupleFields(arity: Int): String = (1 to arity).map(n => "x._" + n).mkString(", ")

  def listPatternMatch(arity: Int): String = ((1 to arity).map(n => "c" + arityChars(n).toLowerCase).toList ::: "Nil" :: Nil).mkString(" :: ")

  def jsonStringParams(arity: Int): String = (1 to arity).map(n => "%sn: JsonString".format(arityChars(n).toLowerCase)).mkString(", ")

  def jsonStringParamNames(arity: Int): String = (1 to arity).map(n => "%sn".format(arityChars(n).toLowerCase)).mkString(", ")


  def genDecodeJsons = {
    def decodeJsonContextArities(n: Int): String = (1 to n).map(n => "%s: DecodeJson".format(arityChars(n))).mkString(", ")

    def decodeJsonParams(n: Int): String = (1 to n).map{n =>
      val char = arityChars(n)
      "decode%s: DecodeJson[%s]".format(char.toLowerCase, char)
    }.mkString(", ")

    def content = {
      val tupleDecodes = aritiesExceptOne.map{arity =>
        val forComprehensionLines: String = (1 to arity).map{n =>
          val char = arityChars(n)
          "          x%s <- decode%s(c%s)".format(char.toLowerCase, char.toLowerCase, char.toLowerCase)
        }.mkString("\n")

        val yieldResult = (1 to arity).map(n => "x" + arityChars(n).toLowerCase).mkString(", ")

        """|
           |  implicit def Tuple%sDecodeJson[%s](implicit %s): DecodeJson[(%s)] =
           |    DecodeJson(c =>
           |      c.jdecode[List[HCursor]] flatMap {
           |        case %s => for {
           |%s
           |        } yield (%s)
           |       case _ => DecodeResult.fail("[%s]Tuple%s[%s]", c.history)
           |      })
           |""".format(
                  arity,
                  functionTypeParameters(arity),
                  decodeJsonParams(arity),
                  functionTypeParameters(arity),
                  listPatternMatch(arity),
                  forComprehensionLines,
                  yieldResult,
                  functionTypeParameters(arity),
                  arity,
                  functionTypeParameters(arity)
                ).stripMargin
      }

      val jdecode1 = """|
                        |  def jdecode1[A, X](f: A => X)(implicit decodea: DecodeJson[A]): DecodeJson[X] =
                        |    decodea.map(f)
                        |""".stripMargin
      val jdecodes = aritiesExceptOne.map{arity =>
        """|
           |  def jdecode%s[%s, X](f: (%s) => X)(implicit dx: DecodeJson[(%s)]): DecodeJson[X] =
           |    dx.map(x => f(%s))
           |""".format(
                  arity,
                  functionTypeParameters(arity),
                  functionTypeParameters(arity),
                  functionTypeParameters(arity),
                  tupleFields(arity)
                ).stripMargin
      }

      val jdecode1L =
        """|
           |  def jdecode1L[A: DecodeJson, X](f: A => X)(an: JsonString): DecodeJson[X] =
           |    DecodeJson(x => for {
           |      aa <- x.get[A](an)
           |    } yield f(aa))
           |""".stripMargin


      val jdecodeLs = aritiesExceptOne.map{arity =>
        val forComprehensionLines: String = (1 to arity).map{n =>
          val upperChar = arityChars(n)
          val lowerChar = upperChar.toLowerCase
          "       %s%s <- x.get[%s](%sn)".format(lowerChar, lowerChar, upperChar, lowerChar)
        }.mkString("\n")

        val yieldExpression: String = (1 to arity).map{n =>
          val lowerChar = arityChars(n).toLowerCase
          "%s%s".format(lowerChar, lowerChar)
        }.mkString(", ")

        """|
           |  def jdecode%sL[%s, X](f: (%s) => X)(%s): DecodeJson[X] =
           |    DecodeJson(x => for {
           |%s
           |    } yield f(%s))
           |""".format(
                  arity,
                  decodeJsonContextArities(arity),
                  functionTypeParameters(arity),
                  jsonStringParams(arity),
                  forComprehensionLines,
                  yieldExpression
                ).stripMargin
      }

      (tupleDecodes ++ (jdecode1 +: jdecodes) ++ (jdecode1L +: jdecodeLs)).mkString
    }
    header +
      """|
         |trait GeneratedDecodeJsons {
         |  this: DecodeJsons =>
         |  import Json._
         |%s
         |}
         |""".format(content).stripMargin
  }

  def genEncodeJsons = {
    def encodeJsonContextArities(n: Int): String = (1 to n).map(n => "%s: EncodeJson".format(arityChars(n))).mkString(", ")

    def encodeJsonParams(n: Int): String = (1 to n).map{n =>
      val char = arityChars(n)
      "encode%s: EncodeJson[%s]".format(char.toLowerCase, char)
    }.mkString(", ")

    def invokeEncodeJsonParams(n: Int): String = (1 to n).map{n =>
      val char = arityChars(n).toLowerCase
      "encode%s(%s)".format(char, char)
    }.mkString(", ")

    def content = {
      val tupleEncodes = aritiesExceptOne.map{arity =>
        """|
           |  implicit def Tuple%sEncodeJson[%s](implicit %s): EncodeJson[(%s)] =
           |    EncodeJson({
           |     case (%s) => jArray(List(%s))
           |    })
           |""".format(
                  arity,
                  functionTypeParameters(arity),
                  encodeJsonParams(arity),
                  functionTypeParameters(arity),
                  functionTypeParameters(arity).toLowerCase,
                  invokeEncodeJsonParams(arity)
                ).stripMargin
      }

      val jencode1 = """|
                        |  def jencode1[X, A](f: X => A)(implicit encodea: EncodeJson[A]): EncodeJson[X] =
                        |    encodea.contramap(f)
                        |""".stripMargin

      val jencodes = aritiesExceptOne.map{arity =>
        """|
           |  def jencode%s[X, %s](f: X => (%s))(implicit encodex: EncodeJson[(%s)]): EncodeJson[X] =
           |    encodex.contramap(f)
           |""".format(
                  arity,
                  functionTypeParameters(arity),
                  functionTypeParameters(arity),
                  functionTypeParameters(arity)
                ).stripMargin
      }

      val jencode1L =
        """|
           |  def jencode1L[X, A](f: X => A)(an: JsonString)(implicit encodea: EncodeJson[A]): EncodeJson[X] =
           |    EncodeJson(x => jSingleObject(an, encodea.apply(f(x))))
           |""".stripMargin

      val jencodeLs = aritiesExceptOne.map{arity =>
        val encodePairs = (1 to arity).map{n =>
          val upperChar = arityChars(n)
          val lowerChar = upperChar.toLowerCase
          "(%sn, encode%s.apply(%s))".format(lowerChar, lowerChar, lowerChar)
        }.mkString(", ")

        """|
           |  def jencode%sL[X, %s](fxn: X => (%s))(%s)(implicit %s): EncodeJson[X] =
           |    EncodeJson(x => jObjectAssocList({
           |      val (%s) = fxn(x)
           |      List(%s)
           |    }))
           |""".format(
                  arity,
                  functionTypeParameters(arity),
                  functionTypeParameters(arity),
                  jsonStringParams(arity),
                  encodeJsonParams(arity),
                  functionTypeParameters(arity).toLowerCase,
                  encodePairs
                ).stripMargin
      }

      (tupleEncodes ++ (jencode1 +: jencodes) ++ (jencode1L +: jencodeLs)).mkString
    }

    header +
      """|
         |trait GeneratedEncodeJsons {
         |  this: EncodeJsons =>
         |  import Json._
         |%s
         |}
         |""".format(content).stripMargin
  }

  def genCodecJsons = {
    def codecJsonContextArities(n: Int): String = (1 to n).map(n => "%s: EncodeJson: DecodeJson".format(arityChars(n))).mkString(", ")

    def content = {

      val codec1 =
        """|
           |  def codec1[A: EncodeJson: DecodeJson, X](f: A => X, g: X => A)(an: JsonString): CodecJson[X] =
           |    CodecJson(jencode1L(g)(an).encode, jdecode1L(f)(an).decode)
           |""".stripMargin


      val codecs = aritiesExceptOne.map{arity =>
        """|
           |  def codec%s[%s, X](f: (%s) => X, g: X => (%s))(%s): CodecJson[X] =
           |    CodecJson(jencode%sL(g)(%s).encode, jdecode%sL(f)(%s).decode)
           |""".format(
                  arity,
                  codecJsonContextArities(arity),
                  functionTypeParameters(arity),
                  functionTypeParameters(arity),
                  jsonStringParams(arity),
                  arity,
                  jsonStringParamNames(arity),
                  arity,
                  jsonStringParamNames(arity)
                ).stripMargin
      }

      val casecodec1 =
        """|
           |  def casecodec1[A: EncodeJson: DecodeJson, X](f: A => X, g: X => Option[A])(an: JsonString): CodecJson[X] =
           |    CodecJson(jencode1L(g)(an).encode, jdecode1L(f)(an).decode)
           |""".stripMargin


      val casecodecs = aritiesExceptOne.map{arity =>
        """|
           |  def casecodec%s[%s, X](f: (%s) => X, g: X => Option[(%s)])(%s): CodecJson[X] =
           |    CodecJson(jencode%sL(g andThen (_.get))(%s).encode, jdecode%sL(f)(%s).decode)
           |""".format(
                  arity,
                  codecJsonContextArities(arity),
                  functionTypeParameters(arity),
                  functionTypeParameters(arity),
                  jsonStringParams(arity),
                  arity,
                  jsonStringParamNames(arity),
                  arity,
                  jsonStringParamNames(arity)
                ).stripMargin
      }

      ((codec1 +: codecs) ++ (casecodec1 +: casecodecs)).mkString
    }
    header +
      """|
         |trait GeneratedCodecJsons {
         |  import Json._
         |  import DecodeJson._
         |  import EncodeJson._
         |%s
         |}
         |""".format(content).stripMargin
  }

}
