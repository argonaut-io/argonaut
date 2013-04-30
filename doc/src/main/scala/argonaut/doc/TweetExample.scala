package argonaut.doc

import scalaz._, Scalaz._
import argonaut._, Argonaut._


object CompleteExample extends App {
  val url = "http://search.twitter.com/search.json?q=argonaut"
  val string = scala.io.Source.fromURL(url).mkString // Don't do this at home

  // Parse and decode the string to your data type using its codec.
  val message = string.decodeOption[Tweets] match {
    case None => "Could not decode Tweets."
    case Some(tweets) => "Decoded %d tweets, with %d retweets.".format(
      tweets.results.size,
      tweets.results.map(_.retweets).suml
    )
  }

  println(message)
}

case class Tweet(createdAt: String, user: String, text: String, retweets: Int)

object Tweet {
  // Complete decoder definition. Manually using zipper to traverse.
  implicit def TweetDecodeJson: DecodeJson[Tweet] =
    DecodeJson(c => for {
        createdAt <- c.get[String]("created_at")
        user <- c.get[String]("from_user")
        text <- c.get[String]("text")
        retweets <- (c --\ "metadata" --\ "recent_retweets").jdecode[Option[Int]]
      } yield Tweet(createdAt, user, text, retweets.getOrElse(0)))
}


case class Tweets(page: Int, next: String, query: String, results: List[Tweet])

object Tweets {
  // Simple decoder definition. Codec defined by inferenced types.
  implicit def TweetsDecodeJson: DecodeJson[Tweets] =
    jdecode4L(Tweets.apply)("page", "next_page", "query", "results")
}
