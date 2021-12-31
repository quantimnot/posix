import tables
import scrape_common, scrape_defs
import ../posix/codes

proc main =
  var errors: OrderedTable[string, tuple[doc: string, codes: set[Code], mayHaveSameValueAs: string]]
  var body = loadHtml(errorCodesPath).child("html").child("body")
  var name, desc: string
  printGeneratedByComment("errors")
  echo """
# This file contains the POSIX errors that is used by the rest of the
# scraper and code generation.
#
# This is parsed from both the `errno.h` header spec and the chapter 2 `2.3 Error Numbers`
# section. There is more information about each error in `2.3 Error Numbers`. `errno.h`
# is only parsed to assert the list of errors are the same.

import std/options
import codes
"""

  let mayHaveSameValueAsPeg = peg"@ 'may assign the same values for [' [A-Z0-9]+ '] and [' {@} ']'"
  for dl in body.between(("h3", some "@ 'Error Numbers'"), ("dl", none string), ("h4", none string)):
    for (dt, dd, codes) in dl.defs():
      let dt = dt[1..^2]
      doAssert errors.hasKey(dt) == false, "There shouldn't be duplicate error symbols"
      var doc: string
      for line in dd.splitLines:
        if not line.isEmptyOrWhitespace:
          doc.add line
      var mayHaveSameValueAs: array[1, string]
      discard doc.match(mayHaveSameValueAsPeg, mayHaveSameValueAs)
      errors[dt] = (doc, codes, mayHaveSameValueAs[0])

  # assert error symbols match between `2.3 Error Numbers` and `errno.h`
  body = loadHtml(headerPath("errno.h")).child("html").child("body")
  let dl = body.querySelector("dl")
  for (dt, dd, codes) in dl.defs():
    doAssert errors.hasKey(dt[1..^2])

  var enumDef = "type Error* {.pure.} = enum\n"
  var mayHaveSameValueAsDef = """
func mayHaveSameValueAs*(error: Error): Option[Error] =
  case error
"""
  var errorMarginCodes = """
func errorMarginCodes*(error: Error): set[Code] =
  case error
"""

  for (ident, details) in errors.pairs:
    enumDef.addLine "  " & ident
    enumDef.addLine "    ## " & details.doc
    if details.mayHaveSameValueAs.len > 0:
      mayHaveSameValueAsDef.addLine "  of Error." & ident & ": return some Error." & details.mayHaveSameValueAs
    if details.codes.len > 0:
      var codes: string
      for code in details.codes:
        codes.add "Code." & $code & ", "
      codes = codes[0..^3]
      errorMarginCodes.addLine "  of Error." & ident & ": return {" & codes & '}'
  mayHaveSameValueAsDef.addLine "  else: discard"
  errorMarginCodes.addLine "  else: discard"

  echo enumDef
  echo mayHaveSameValueAsDef
  echo errorMarginCodes

main()
