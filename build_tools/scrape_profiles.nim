import scrape_common

proc main =
  var body = loadHtml(conformancePath).child("html").child("body")
  printGeneratedByComment("Conformance")
  echo """
## This file contains the POSIX profile enum that is used by the rest of the
## scraper and code generation.
## This enum is also used in the final code module.

type Profile* {.pure.} = enum
  None = "NONE""""
  var offset = 0
  let text = body.innerText
  var matches: array[1, string]
  while true:
    offset = text.find(peg"'marked ' 'with '? {[A-Z ]+}", matches, offset)
    if offset == -1:
      break
    else:
      offset.inc
      echo indent(matches[0].normalizeEnumIdent & " = \"" & matches[0] & '"', 2)

main()
