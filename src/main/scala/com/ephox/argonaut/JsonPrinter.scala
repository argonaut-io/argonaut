package com.ephox.argonaut

object JsonPrinter extends Application{
  def compact(json: Json): String = prettyPrint(json, "", "", "")

  def pretty(json: Json): String = prettyPrint(json, "    ", "\n",  "")

  // FIX Naive test to get some testing happening... delete. Clean up.  
  def prettyPrint(json: Json, tab:String, delimbreak: String, currentindent: String): String = {

    def join(s: List[String]) = s.mkString("," + delimbreak + currentindent + tab)

    def recurse(json: Json) = prettyPrint(json, tab, delimbreak, currentindent + tab)

    json.fold (
      "null",
      _.toString,
      _.toString,
      printString,
      xs => "[" + delimbreak + currentindent + tab +
          join(xs.map(recurse(_))) +
       delimbreak + currentindent + "]",
      xs => "{" + delimbreak + currentindent + tab +
          join(xs.map({case (k, v) => printString(k) + ": " + recurse(v)})) +
       delimbreak + currentindent + "}"
    )
  }

  def printString(s: String) =  '"' + escape(s) + '"'



  def escape(s: String) =
    s.replace("\\", "\\\\").
      replace("\"", "\\\"").
      replace("\b", "\\b").
      replace("\f", "\\f").
      replace("\n", "\\n").
      replace("\r", "\\r").
      replace("\t", "\\t")
}