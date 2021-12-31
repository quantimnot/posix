import options, pegs, htmlparser, xmltree, strutils
import strtabs  # To access XmlAttributes
# from strutils import parseEnum, contains, toLowerAscii, splitLines, repeat, strip, replace, indent
# export parseEnum, contains, toLowerAscii, splitLines, repeat, strip, replace, indent
import nimquery

when isMainModule:
  import unittest

when defined posix_2004:
  import posix_2004 as posix_impl
elif defined posix_2008:
  import posix_2008 as posix_impl
elif defined posix_2013:
  import posix_2013 as posix_impl
elif defined posix_2016:
  import posix_2016 as posix_impl
else:
  import posix_2018 as posix_impl

export options, pegs, htmlparser, xmltree, strtabs, nimquery, posix_impl, strutils

func isEmptyOrWhitespace*(element: XmlNode): bool =
  element.innerText.len == 0 or element.innerText.isEmptyOrWhitespace

type Selector* = tuple
  cssSelector: string
  contentPeg: Option[string]

proc q*(n: XmlNode, sub: string): XmlNode {.raises: []} =
  for child in n:
    if child.kind == xnElement:
      echo child.tag
      if child.innerText.contains(sub):
        return child

proc q*(n: XmlNode, sel: Selector): XmlNode {.raises: [ValueError, Exception]} =
  if sel.contentPeg.isSome:
    let matches = n.querySelectorAll(sel.cssSelector)
    for m in matches:
      if m.innerText.match(sel.contentPeg.get.parsePeg):
        result = m
        break
  else:
    result = n.querySelector(sel.cssSelector)
  if result.isNil:
    raise newException(ValueError, "no element matches query")

proc matches*(n: XmlNode, sel: Selector): bool {.raises: [ParseError, ref ValueError, Exception]} =
  if sel.cssSelector.len > 0:
    if not n.querySelector(sel.cssSelector).isNil:
      if sel.contentPeg.isSome:
        if n.innerText.match(sel.contentPeg.get.parsePeg):
          return true
      else:
        return true
  elif sel.contentPeg.isSome:
    if n.innerText.match(sel.contentPeg.get.parsePeg):
      return true

proc matches*(n: XmlNode, sel: Selector, captures: var seq[string]): bool {.raises: [ParseError, ref ValueError, Exception]} =
  if not n.querySelector(sel.cssSelector).isNil:
    if sel.contentPeg.isSome:
      if n.innerText =~ sel.contentPeg.get.parsePeg:
        for m in matches:
          if m.len > 0:
            captures.add m
        return true
    else:
      return true

template splitLinesOfNextElement*(a, b) =
  n = c.nextMatchIdx(n, a).get
  n = c.nextIdx(n+1).get
  for c in c[n]:
    if c.kind == xnText:
      for l in c.innerText.splitLines:
        var l = l.strip(chars = {' ', '\n'})
        if not l.isEmptyOrWhitespace:
          b(l)


iterator betweenAll*(n: XmlNode, start, query, stop: Selector, offset = 0): XmlNode {.raises: [ValueError, Exception]} =
  var foundStart = false
  for i in offset .. n.len-1:
    template c: untyped = n[i]
    if foundStart:
      if c.matches(stop):
        foundStart = false
      elif c.matches(query):
        case c.kind:
        of xnElement:
          yield c
        of xnText:
          if not c.isEmptyOrWhitespace:
            yield c
        else: discard
    else:
      foundStart = c.matches(start)


iterator between*(n: XmlNode, start, query, stop: Selector): XmlNode {.raises: [ValueError, Exception]} =
  var foundStart = false
  for c in n:
    if foundStart:
      if c.matches(stop):
        break
      elif c.matches(query):
        case c.kind:
        of xnElement:
          yield c
        of xnText:
          if not c.isEmptyOrWhitespace:
            yield c
        else: discard
    else:
      foundStart = c.matches(start)

iterator betweenAll*(n: XmlNode, start, stop: Selector): XmlNode {.raises: [ValueError, Exception]} =
  var foundStart = false
  for c in n:
    if foundStart:
      if c.matches(stop):
        foundStart = false
      else:
        case c.kind:
        of xnElement:
          yield c
        of xnText:
          if not c.isEmptyOrWhitespace:
            yield c
        else: discard
    else:
      foundStart = c.matches(start)

iterator between*(n: XmlNode, start, stop: Selector): XmlNode {.raises: [ValueError, Exception]} =
  var foundStart = false
  for c in n:
    if foundStart:
      if c.matches(stop):
        break
      else:
        case c.kind:
        of xnElement:
          yield c
        of xnText:
          if not c.isEmptyOrWhitespace:
            yield c
        else: discard
    else:
      foundStart = c.matches(start)

iterator next*(n: XmlNode, i: var int, kind = xnElement): int =
  while i < n.len:
    if n[i].kind == kind:
      yield i
    i.inc

func nextIdx*(node: XmlNode, offset: int, cb: proc(n:XmlNode):bool): Option[int] =
  assert offset < node.len-1
  for nextIdx in offset..node.len-1:
    if node[nextIdx].cb:
      return some nextIdx

func nextIdx*(node: XmlNode, offset: int, kind = xnElement): Option[int] =
  assert offset < node.len-1
  for nextIdx in offset..node.len-1:
    if node[nextIdx].kind == kind:
      return some nextIdx

proc nextMatchIdx*(node: XmlNode, offset: int, sel: Selector): Option[int] {.raises: [ValueError, Exception]} =
  assert offset < node.len-1
  for nextIdx in offset..node.len-1:
    if node[nextIdx].matches(sel):
      return some nextIdx

func normalizeEnumIdent*(s: string, sep = ' '): string =
  var beginWord = true
  for c in s:
    if c == sep:
      beginWord = true
    elif beginWord:
      result.add c
      beginWord = false
    else:
      result.add c.toLowerAscii

template basenames*(glob: string): string =
  walkFiles(glob)

iterator functions*(glob: string): string =
  for f in basenames(glob):
    yield f.splitFile.name

iterator basedefs*(glob: string): tuple[name: string, body: var XmlNode] =
  for f in basenames(glob):
    yield (f.splitFile.name, loadHtml(f).child("html").child("body"))

proc loadHeaderHtml*(name: string): XmlNode =
  headerPath(name).loadHtml.child("html").child("body")

func skipToMeaningfulElementOrText*(element: XmlNode, nextChildIdx: var int) =
  template e: untyped = element
  template i: untyped = nextChildIdx
  if nextChildIdx < e.len:
    if (e[i].kind == xnText and e[i].isEmptyOrWhitespace):
      i.inc
    elif e[i].kind in {xnComment, xnEntity, xnCData} or
        (e[i].kind == xnElement and e[i].htmlTag == tagBr):
      i.inc
      element.skipToMeaningfulElementOrText(i)
when isMainModule:
  suite "parse":
    test "skipToMeaningfulElementOrText":
      var i = 0
      var html = parseHtml("""<p></p>""")
      html.skipToMeaningfulElementOrText(i)
      check i == 0
      i = 0
      html = parseHtml("""
      <p></p>""")
      html.skipToMeaningfulElementOrText(i)
      check i == 1
      i = 0
      html = parseHtml("""<br><p></p>""")
      html.skipToMeaningfulElementOrText(i)
      check i == 1
      i = 0
      html = parseHtml("""<br><br><i></i>""")
      html.skipToMeaningfulElementOrText(i)
      check i == 2
      i = 0
      html = parseHtml("""0<p></p>""")
      html.skipToMeaningfulElementOrText(i)
      check i == 0
      i = 0
      html = parseHtml("""<dl compact>
<dt>FD_CLOEXEC</dt>

<dd>Close the file descriptor upon execution of an <i>exec</i> family function.</dd>
</dl>

<p>The <i>&lt;fcntl.h&gt;</i> header shall also define the following symbolic constants for the <i>l_type</i> argument used for
record locking with <a href="../functions/fcntl.html"><i>fcntl</i>()</a>. The values shall be unique and shall be suitable for use
in <b>#if</b> preprocessing directives.</p>""")
      html.skipToMeaningfulElementOrText(i)
      check i == 0
      i.inc
      html.skipToMeaningfulElementOrText(i)
      check i == 2
      check html[i].htmlTag == tagP

type CExtension* {.pure.} = enum
  Independent
  Aligns
  Extends
func parseCx*(element: XmlNode, nextChildIdx: var int): CExtension =
  template e: untyped = element
  if e.htmlTag == tagDiv and e.attr("class") == "box":
    if e.innerText.contains("extends the ISO"):
      nextChildIdx.inc
      return Extends
    if e.innerText.contains("aligned with"):
      nextChildIdx.inc
      return Aligns
    doAssert false, "unhandled `box` div"
when isMainModule:
  suite "parse":
    test "parseCx":
      var i = 0
      var html = parseHtml("""<div class="box"></div>""")
      try:
        discard html.parseCx(i)
        check false # "assert unhandled `box` div"
      except: discard

      html = parseHtml("""<div></div>""")
      check html.parseCx(i) == CExtension.Independent
      check i == 0

      i = 0
      html = parseHtml("""<div class="box"><sup>[<a href="javascript:open_code('CX')">CX</a>]</sup> <img src="../images/opt-start.gif" alt="[Option Start]" border="0"> Some of the functionality described on this reference page extends the ISO&nbsp;C standard. Applications shall define
the appropriate feature test macro (see XSH <a href="../functions/V2_chap02.html#tag_15_02"><i>The Compilation Environment</i></a>
) to enable the visibility of these symbols in this header. <img src="../images/opt-end.gif" alt="[Option End]" border="0"></div>""")
      check html.parseCx(i) == CExtension.Extends
      check i == 1

      i = 0
      html = parseHtml("""<div class="box"><sup>[<a href="javascript:open_code('CX')">CX</a>]</sup> <img src="../images/opt-start.gif" alt="[Option Start]" border="0"> The functionality described on this reference page is aligned with the ISO&nbsp;C standard. Any conflict between the
requirements described here and the ISO&nbsp;C standard is unintentional. This volume of POSIX.1-2017 defers to the ISO&nbsp;C
standard. <img src="../images/opt-end.gif" alt="[Option End]" border="0"></div>""")
      check html.parseCx(i) == CExtension.Aligns
      check i == 1


# type ParagraphKind* {.pure.} = enum
#   Unknown
#   Supplementary
#   DefinesFollowingStructs
#   DefinesFollowingUnions
#   DefinesFollowingConstants
#   DefinesFollowingVars
#   DefinesFollowingFunctions
#   DefinesFollowingTypedefs
#   DefinesInlineTypedefs
#   DefinesImportedSymbols
# proc classifyParagraph*(element: XmlNode, nextChildIdx: var int): ParagraphKind =
#   template e: untyped = element
#   if e.tryParseHeaderConsts(nextChildIdx):
#     return DefinesFollowingConstants
#   elif e.tryparseHeaderVars(nextChildIdx):
#     return DefinesFollowingVars
#   elif e.tryparseHeaderTypedefs(nextChildIdx):
#     return DefinesFollowingTypedefs
#   elif e.tryparseHeaderInlineTypedefs(nextChildIdx):
#     return DefinesInlineTypedefs
#   elif e.tryparseHeaderStructs(nextChildIdx):
#     return DefinesFollowingStructs
#   elif e.tryparseHeaderUnions(nextChildIdx):
#     return DefinesFollowingUnions
#   elif e.tryparseImports(nextChildIdx):
#     return DefinesImportedSymbols
#   elif e.tryparseFuncs(nextChildIdx):
#     return DefinesFollowingFunctions
#   elif e.tryMayMakeVisible(nextChildIdx):
#     return Supplementary
