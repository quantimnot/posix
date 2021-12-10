import scrape_common
import tables, sequtils, sets

proc main =
  let body = loadHtml(subprofilesPath).child("html").child("body")
  printGeneratedByComment("Subprofiling Considerations (Informative)")
  echo """
## This file contains the POSIX sub-profile enum that is used by the rest of the
## scraper and code generation.

import sets

type SubProfile* {.pure.} = enum"""
  let subProfileNameDescPeg = """{@} ':' \s {@} $""".peg
  let dl = body.querySelector("dl")
  var matches: array[2, string]
  var subProfileFuncs: seq[seq[string]]
  var subProfileDescs: seq[string]
  var ident: string
  for (term, funcs) in dl.defs:
    doAssert term.match(subProfileNameDescPeg, matches)
    ident = matches[0].normalizeEnumIdent('_')
    echo indent(ident & " ## " & matches[1], 2)
    subProfileFuncs.add funcs.split(", ").mapIt(it[0..^3])
    subProfileDescs.add matches[1]

  echo "\nconst subProfileFuncs* = ["
  for fns in subProfileFuncs:
    echo indent($fns, 2) & ".toHashSet,"
  echo ']'

  echo "\nconst subProfileDescs* = ["
  for desc in subProfileDescs:
    echo indent('"' & desc & '"', 2) & ','
  echo ']'

  echo """

func subProfile*(fn: string): SubProfile {.raises: [ValueError].} =
  var n = 0
  for funcs in subProfileFuncs:
    if fn in funcs:
      return SubProfile(n)
    n.inc
  raise newException(ValueError, fn & " doesn't exist")"""

main()
