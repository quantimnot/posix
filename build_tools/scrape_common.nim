import strutils, options, pegs, htmlparser, xmltree
import nimquery

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

export strutils, options, pegs, htmlparser, xmltree, nimquery, posix_impl

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
  if not n.querySelector(sel.cssSelector).isNil:
    if sel.contentPeg.isSome:
      if n.innerText.match(sel.contentPeg.get.parsePeg):
        return true
    else:
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

iterator defs*(n: XmlNode): tuple[term, desc: string] =
  var result: tuple[term, desc: string]
  for e in n:
    case e.kind:
    of xnElement:
      case e.tag
      of "dt":
        result.term = e.innerText
        result.term = result.term.replace('\n', ' ')
        result.term = result.term.strip
      of "dd":
        result.desc = e.innerText
        result.desc = result.desc.replace('\n', ' ')
        result.desc = result.desc.strip
        yield result
    else: discard

template splitLinesOfNextElement*(a, b) =
  n = c.nextMatchIdx(n, a).get
  n = c.nextIdx(n+1).get
  for c in c[n]:
    if c.kind == xnText:
      for l in c.innerText.splitLines:
        var l = l.strip(chars = {' ', '\n'})
        if not l.isEmptyOrWhitespace:
          b(l)

iterator betweenAll*(n: XmlNode, start, query, stop: Selector): XmlNode {.raises: [ValueError, Exception]} =
  var foundStart = false
  for c in n:
    if foundStart:
      if c.matches(stop):
        foundStart = false
      elif c.matches(query):
        case c.kind:
        of xnElement:
          yield c
        of xnText:
          if not isEmptyOrWhitespace($c):
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
          if not isEmptyOrWhitespace($c):
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
          if not isEmptyOrWhitespace($c):
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
          if not isEmptyOrWhitespace($c):
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

template docLine*(s: var string) =
  s.add "##\n"

template addLine*(s: var string, t: string) =
  s.add t & '\n'

template docHeader*(s: var string, t: string) =
  s.add "## " & t & '\n'

template doc*(s: var string, t: string, indentLevel = 0) =
  s.add "##   " & indent(t, indentLevel) & '\n'

func doc*(t: string, indentLevel = 0): string =
  for l in t.splitLines:
    result.add "## " & repeat(' ', indentLevel) & l

proc loadHeaderHtml*(name: string): XmlNode =
  headerPath(name).loadHtml.child("html").child("body")

template printGeneratedByComment*(source: string, commentLeader = "#") =
  echo commentLeader & " GENERATED from the \"" & source & "\" page of the POSIX html spec by `" & instantiationInfo().filename & '`'
