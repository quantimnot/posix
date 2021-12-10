import scrape_common

proc main =
  printGeneratedByComment("unistd.h")
  echo "type Option* {.pure.} = enum"
  var body = loadHeaderHtml("unistd.h")
  var c = body.nextMatchIdx(0, ("h4", some "'DESCRIPTION'")).get
  c = body.nextIdx(c+1).get
  for e in body[c].betweenAll(("h5", some "'Constants for Options and Option Groups'"), ("dl", string.none), ("h5", some "'Execution-Time Symbolic Constants'")):
    for (k, _) in e.defs():
      echo indent(k[1..^1].normalizeEnumIdent('_') & " ## " & k, 2)

main()
