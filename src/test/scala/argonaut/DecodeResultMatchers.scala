package argonaut

import org.specs2.execute.Failure
import org.specs2.matcher.{MatchResult, Matcher, MatchersImplicits}

object DecodeResultMatchers extends MatchersImplicits {
  def beOk[A]: Matcher[DecodeResult[A]] = (result: DecodeResult[A]) =>
    (!result.isError, s"Error: $result")

  def beOkLike[A](test: A => MatchResult[A]): Matcher[DecodeResult[A]] = (result: DecodeResult[A]) =>
    result.fold(
      (_, _) => Failure(s"Error: $result"),
      a      => test(a).toResult
    )

  def beError[A]: Matcher[DecodeResult[A]] = (result: DecodeResult[A]) =>
    (result.isError, s"$result is not an error")

  def beErrorWithMessage[A](message: String): Matcher[DecodeResult[A]] = (result: DecodeResult[A]) =>
    result.fold(
      (m, h) => (m contains message, s"$m doesn't contain $message"),
      _      => (false,  s"$result is not an error")
    )
}
