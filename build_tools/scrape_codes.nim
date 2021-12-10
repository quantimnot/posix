import scrape_common

proc main =
  var body = loadHtml(codesPath).child("html").child("body")
  var name, desc: string
  printGeneratedByComment("codes")
  echo """
# This file contains the POSIX profile enum that is used by the rest of the
# scraper and code generation.
# This enum is also used in the final code module.

type Code* {.pure.} = enum"""
  for n in body.between(("h4:first-of-type", string.none), ("h4:last-of-type", string.none)):
    let e = n.querySelector("p > sup > a[name]:last-of-type")
    if not e.isNil:
      name = e.attr("name")
      for n in n.between(("img:first-of-type", string.none), ("img:last-of-type", string.none)):
        desc.add ($n).strip.replace('\n', ' ') & ' '
      desc.delete(desc.len-1, desc.len-1)
      echo indent(name & " ## " & desc, 2)
      name.reset
      desc.reset

main()
