import scrape_common, parse_codes, scrape_defs
import tables

proc main =
  var codeToOption: OrderedTable[Code, string]
  printGeneratedByComment("unistd.h")
  echo "type PosixOption* {.pure.} = enum"
  var body = loadHeaderHtml("unistd.h")
  var c = body.nextMatchIdx(0, ("h4", some "'DESCRIPTION'")).get
  c = body.nextIdx(c+1).get
  for e in body[c].betweenAll(("h5", some "'Constants for Options and Option Groups'"), ("dl", string.none), ("h5", some "'Execution-Time Symbolic Constants'")):
    for (ident, doc, codes) in e.defs():
      let normIdent = ident[1..^1].normalizeEnumIdent('_')
      echo indent(normIdent, 2)
      echo indent("## " & ident, 4)
      echo indent("## " & doc, 4)
      for code in codes:
        if code != Code.OB:
          codeToOption[code] = normIdent
  echo """

import codes

func codeToOption*(code: Code): PosixOption =
  case code"""
  for (code, option) in codeToOption.pairs:
    echo "  of Code." & $code & ": return PosixOption." & option
  echo "  else: discard"

main()
