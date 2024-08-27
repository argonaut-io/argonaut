import sbt.*

object Boilerplate {
  val arities = 1 to 22
  val aritiesExceptOne = 2 to 22
  val arityChars: Map[Int, String] = arities.map(n => (n, ('A' + n - 1).toChar.toString)).toMap

  def write(path: File, fileContents: String): File = {
    IO.write(path, fileContents)
    path
  }

  def gen(dir: File) = {
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

  def listPatternMatch(arity: Int): String =
    ((1 to arity).map(n => "c" + arityChars(n).toLowerCase).toList ::: "Nil" :: Nil).mkString(" :: ")

  def jsonStringParams(arity: Int): String =
    (1 to arity).map(n => s"${arityChars(n).toLowerCase}n: JsonString").mkString(", ")

  def jsonStringParamNames(arity: Int): String = (1 to arity).map(n => s"${arityChars(n).toLowerCase}n").mkString(", ")

  def genDecodeJsons = {
    def decodeJsonContextArities(n: Int): String = (1 to n).map(n => s"${arityChars(n)}: DecodeJson").mkString(", ")

    def decodeJsonParams(n: Int): String = (1 to n).map { n =>
      val char = arityChars(n)
      s"decode${char.toLowerCase}: DecodeJson[${char}]"
    }.mkString(", ")

    def content = {
      val tupleDecodes = aritiesExceptOne.map { arity =>
        val forComprehensionLines: String = (1 to arity).map { n =>
          val char = arityChars(n)
          s"          x${char.toLowerCase} <- decode${char.toLowerCase}(c${char.toLowerCase})"
        }.mkString("\n")

        val yieldResult = (1 to arity).map(n => "x" + arityChars(n).toLowerCase).mkString(", ")

        s"""|
            |  implicit def Tuple${arity}DecodeJson[${functionTypeParameters(arity)}](implicit ${decodeJsonParams(
            arity
          )}): DecodeJson[(${functionTypeParameters(arity)})] =
            |    DecodeJson(c =>
            |      c.jdecode[List[HCursor]] flatMap {
            |        case ${listPatternMatch(arity)} => for {
            |${forComprehensionLines}
            |        } yield (${yieldResult})
            |       case _ => DecodeResult.fail("[${functionTypeParameters(
            arity
          )}]Tuple${arity}[${functionTypeParameters(arity)}]", c.history)
            |      })
            |""".stripMargin
      }

      val jdecode1 = """|
                        |  def jdecode1[A, X](f: A => X)(implicit decodea: DecodeJson[A]): DecodeJson[X] =
                        |    decodea.map(f)
                        |""".stripMargin
      val jdecodes = aritiesExceptOne.map { arity =>
        s"""|
            |  def jdecode${arity}[${functionTypeParameters(arity)}, X](f: (${functionTypeParameters(
            arity
          )}) => X)(implicit dx: DecodeJson[(${functionTypeParameters(arity)})]): DecodeJson[X] =
            |    dx.map(x => f(${tupleFields(arity)}))
            |""".stripMargin
      }

      val jdecode1L =
        """|
           |  def jdecode1L[A: DecodeJson, X](f: A => X)(an: JsonString): DecodeJson[X] =
           |    DecodeJson(x => for {
           |      aa <- x.get[A](an)
           |    } yield f(aa))
           |""".stripMargin

      val jdecodeLs = aritiesExceptOne.map { arity =>
        val forComprehensionLines: String = (1 to arity).map { n =>
          val upperChar = arityChars(n)
          val lowerChar = upperChar.toLowerCase
          s"       ${lowerChar}${lowerChar} <- x.get[${upperChar}](${lowerChar}n)"
        }.mkString("\n")

        val yieldExpression: String = (1 to arity).map { n =>
          val lowerChar = arityChars(n).toLowerCase
          s"${lowerChar}${lowerChar}"
        }.mkString(", ")

        s"""|
            |  def jdecode${arity}L[${decodeJsonContextArities(arity)}, X](f: (${functionTypeParameters(
            arity
          )}) => X)(${jsonStringParams(arity)}): DecodeJson[X] =
            |    DecodeJson(x => for {
            |${forComprehensionLines}
            |    } yield f(${yieldExpression}))
            |""".stripMargin
      }

      (tupleDecodes ++ (jdecode1 +: jdecodes) ++ (jdecode1L +: jdecodeLs)).mkString
    }
    header +
      s"""|
          |trait GeneratedDecodeJsons {
          |  this: DecodeJsons =>
          |  import Json._
          |${content}
          |}
          |""".stripMargin
  }

  def genEncodeJsons = {
    def encodeJsonContextArities(n: Int): String = (1 to n).map(n => s"${arityChars(n)}: EncodeJson").mkString(", ")

    def encodeJsonParams(n: Int): String = (1 to n).map { n =>
      val char = arityChars(n)
      s"encode${char.toLowerCase}: EncodeJson[${char}]"
    }.mkString(", ")

    def invokeEncodeJsonParams(n: Int): String = (1 to n).map { n =>
      val char = arityChars(n).toLowerCase
      s"encode${char}(${char})"
    }.mkString(", ")

    def content = {
      val tupleEncodes = aritiesExceptOne.map { arity =>
        s"""|
            |  implicit def Tuple${arity}EncodeJson[${functionTypeParameters(arity)}](implicit ${encodeJsonParams(
            arity
          )}): EncodeJson[(${functionTypeParameters(arity)})] =
            |    EncodeJson({
            |     case (${functionTypeParameters(arity).toLowerCase}) => jArray(List(${invokeEncodeJsonParams(arity)}))
            |    })
            |""".stripMargin
      }

      val jencode1 = """|
                        |  def jencode1[X, A](f: X => A)(implicit encodea: EncodeJson[A]): EncodeJson[X] =
                        |    encodea.contramap(f)
                        |""".stripMargin

      val jencodes = aritiesExceptOne.map { arity =>
        s"""|
            |  def jencode${arity}[X, ${functionTypeParameters(arity)}](f: X => (${functionTypeParameters(
            arity
          )}))(implicit encodex: EncodeJson[(${functionTypeParameters(arity)})]): EncodeJson[X] =
            |    encodex.contramap(f)
            |""".stripMargin
      }

      val jencode1L =
        """|
           |  def jencode1L[X, A](f: X => A)(an: JsonString)(implicit encodea: EncodeJson[A]): EncodeJson[X] =
           |    EncodeJson(x => jSingleObject(an, encodea.apply(f(x))))
           |""".stripMargin

      val jencodeLs = aritiesExceptOne.map { arity =>
        val encodePairs = (1 to arity).map { n =>
          val upperChar = arityChars(n)
          val lowerChar = upperChar.toLowerCase
          s"(${lowerChar}n, encode${lowerChar}.apply(${lowerChar}))"
        }.mkString(", ")

        s"""|
            |  def jencode${arity}L[X, ${functionTypeParameters(arity)}](fxn: X => (${functionTypeParameters(
            arity
          )}))(${jsonStringParams(arity)})(implicit ${encodeJsonParams(arity)}): EncodeJson[X] =
            |    EncodeJson(x => jObjectAssocList({
            |      val (${functionTypeParameters(arity).toLowerCase}) = fxn(x)
            |      List(${encodePairs})
            |    }))
            |""".stripMargin
      }

      (tupleEncodes ++ (jencode1 +: jencodes) ++ (jencode1L +: jencodeLs)).mkString
    }

    header +
      s"""|
          |trait GeneratedEncodeJsons {
          |  this: EncodeJsons =>
          |  import Json._
          |${content}
          |}
          |""".stripMargin
  }

  def genCodecJsons = {
    def codecJsonContextArities(n: Int): String =
      (1 to n).map(n => s"${arityChars(n)}: EncodeJson: DecodeJson").mkString(", ")

    def content = {

      val codec1 =
        """|
           |  def codec1[A: EncodeJson: DecodeJson, X](f: A => X, g: X => A)(an: JsonString): CodecJson[X] =
           |    CodecJson(jencode1L(g)(an).encode, jdecode1L(f)(an).decode)
           |""".stripMargin

      val codecs = aritiesExceptOne.map { arity =>
        s"""|
            |  def codec${arity}[${codecJsonContextArities(arity)}, X](f: (${functionTypeParameters(
            arity
          )}) => X, g: X => (${functionTypeParameters(arity)}))(${jsonStringParams(arity)}): CodecJson[X] =
            |    CodecJson(jencode${arity}L(g)(${jsonStringParamNames(
            arity
          )}).encode, jdecode${arity}L(f)(${jsonStringParamNames(arity)}).decode)
            |""".stripMargin
      }

      val casecodec1 =
        """|
           |  def casecodec1[A: EncodeJson: DecodeJson, X](f: A => X, g: X => Option[A])(an: JsonString): CodecJson[X] =
           |    CodecJson(jencode1L(g)(an).encode, jdecode1L(f)(an).decode)
           |""".stripMargin

      val casecodecs = aritiesExceptOne.map { arity =>
        s"""|
            |  def casecodec${arity}[${codecJsonContextArities(arity)}, X](f: (${functionTypeParameters(
            arity
          )}) => X, g: X => Option[(${functionTypeParameters(arity)})])(${jsonStringParams(arity)}): CodecJson[X] =
            |    CodecJson(jencode${arity}L(g andThen (_.get))(${jsonStringParamNames(
            arity
          )}).encode, jdecode${arity}L(f)(${jsonStringParamNames(arity)}).decode)
            |""".stripMargin
      }

      ((codec1 +: codecs) ++ (casecodec1 +: casecodecs)).mkString
    }
    header +
      s"""|
          |trait GeneratedCodecJsons {
          |  import Json._
          |  import DecodeJson._
          |  import EncodeJson._
          |${content}
          |}
          |""".stripMargin
  }

}
