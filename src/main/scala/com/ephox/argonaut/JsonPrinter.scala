package com.ephox
package argonaut

object JsonPrinter extends JsonPrinters

trait JsonPrinters {
  def compact(json: Json): String = prettyPrint(json, "", "", "")

  def pretty(json: Json): String = prettyPrint(json, "    ", "\n",  "")

  // FIX simplify this fugly duckling
  def prettyPrint(json: Json, tab:String, delimbreak: String, currentindent: String): String = {

    def join(s: List[String]) = s.mkString("," + delimbreak + currentindent + tab)

    def recurse(json: Json) = prettyPrint(json, tab, delimbreak, currentindent + tab)

    def bracket(s: String, open: String, close: String) =
        open + delimbreak + currentindent + tab + s +  delimbreak + currentindent + close

    def entries[A](entries: List[A], open: String, close: String, f: A => String) =
        bracket(join(entries map f), open, close)

    def printString(s: String) =  '"' + escape(s) + '"'

    def escape(s: String) =
      s.replace("\\", "\\\\").
        replace("\"", "\\\"").
        replace("\b", "\\b").
        replace("\f", "\\f").
        replace("\n", "\\n").
        replace("\r", "\\r").
        replace("\t", "\\t")

    json.fold (
      "null",
      _.toString,
      n =>
        if (math.floor(n) == n && math.round(n).toDouble == n)
          math.round(n).toString
        else
          n.toString,
      printString,
      entries(_, "[", "]", recurse(_:Json)),

      // FIX why does the inferencer fail here, but not for `map`.
      entries(_, "{", "}", {case (k, v) => printString(k) + ": " + recurse(v)}: (((String, Json)) => String))
    )
  }
}
