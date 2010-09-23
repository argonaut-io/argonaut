package com.ephox.argonaut

object JsonPrinter extends Application{
  def compact(json: Json): String = prettyPrint(json, "", "", "")

  def pretty(json: Json): String = prettyPrint(json, "    ", "\n",  "")

  // FIX Naive test to get some testing happening... delete. Clean up.  
  def prettyPrint(json: Json, tab:String, delimbreak: String, currentindent: String): String =
    json.fold (
      "null",
      _.toString,
      _.toString,
      '"' + escape(_) + '"',
      "[" + delimbreak + currentindent + tab +
          _.map(prettyPrint(_, tab, delimbreak, currentindent + tab)).mkString("," + delimbreak + currentindent + tab) +
       delimbreak + currentindent + "]",
      "{" + delimbreak + currentindent + tab +
          _.map({case (k, v) => '"' + escape(k) + "\": " + prettyPrint(v, tab, delimbreak, currentindent + tab)}).mkString("," + delimbreak + currentindent + tab) +
       delimbreak + currentindent + "}"
    )

  def escape(s: String) =
    s.replace("\\", "\\\\").
      replace("\"", "\\\"").
      replace("\b", "\\b").
      replace("\f", "\\f").
      replace("\n", "\\n").
      replace("\r", "\\r").
      replace("\t", "\\t")
}