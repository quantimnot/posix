import pegs, htmlparser, xmltree, strutils
import scrape_common
import ../posix/codes
export codes

when isMainModule:
  import unittest

let marginCodesPeg = peg"{\w+} (' ' {\w+})*"
proc parseCodes(element: XmlNode, codes: var set[Code]): bool =
  if element.innerText =~ marginCodesPeg:
    var i = 0
    while i < matches.len:
      if matches[i].len > 0:
        codes.incl parseEnum[Code](matches[i])
      i.inc
    return true
proc tryParseMarginCodesBeginTag*(element: XmlNode, nextChildIdx: var int, codes: var set[Code]): bool {.discardable.} =
  var i = nextChildIdx
  template e: untyped = element
  if e[i].kind == xnElement:
    if e[i].htmlTag == tagSup:
      if e[i].len == 3:
        if e[i][0].innerText.strip == "[" and
           e[i][1].parseCodes(codes) and
           e[i][2].innerText.strip == "]":
          i.inc
          e.skipToMeaningfulElementOrText(i)
          if e[i].kind == xnElement and
             e[i].htmlTag == tagImg and
             e[i].attr("alt") == "[Option Start]":
              nextChildIdx = i + 1
              e.skipToMeaningfulElementOrText(nextChildIdx)
              doAssert e.len > nextChildIdx, "expected siblings"
              return true
proc tryParseMarginCodesEndTag*(element: XmlNode, nextChildIdx: var int): bool =
  template e: untyped = element
  template i: untyped = nextChildIdx
  if e[i].kind == xnElement and e[i].htmlTag == tagImg and e[i].attr("alt") == "[Option End]":
    i.inc
    return true
when isMainModule:
  suite "parse":
    test "tryParseMarginCodesBeginTag":
      var nextChildIdx = 0
      var codes: set[Code]
      var element =
        parseHtml("""<sup>[<a href="javascript:open_code('OB')">OB</a>]</sup>
<img src="../images/opt-start.gif" alt="[Option Start]" border="0">t""")
      check element.tryParseMarginCodesBeginTag(nextChildIdx, codes)
      check nextChildIdx == 3
      check codes == {Code.OB}

      nextChildIdx = 0
      codes.reset
      element =
        parseHtml("""<sup>[<a href="javascript:open_code('OB CX')">OB CX</a>]</sup><img src="../images/opt-start.gif" alt="[Option Start]" border="0">a""")
      check element.tryParseMarginCodesBeginTag(nextChildIdx, codes)
      check nextChildIdx == 2
      check codes == {Code.OB, Code.CX}

      checkpoint "test assertion of more children than the margin code elements"
      try:
        nextChildIdx = 0
        codes.reset
        element =
          parseHtml("""<sup>[<a href="javascript:open_code('OB CX')">OB CX</a>]</sup><img src="../images/opt-start.gif" alt="[Option Start]" border="0">""")
        discard element.tryParseMarginCodesBeginTag(nextChildIdx, codes)
        check false
      except:
        check true
