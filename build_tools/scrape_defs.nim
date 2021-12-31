import htmlparser, xmltree, strutils
import parse_codes
import ../posix/codes

iterator defs*(n: XmlNode): tuple[term, doc: string, codes: set[Code]] =
  var result: tuple[term, doc: string, codes: set[Code]]
  for e in n:
    case e.kind:
    of xnElement:
      case e.htmlTag
      of tagDt:
        result.term = e.innerText
        result.term = result.term.replace('\n', ' ')
        result.term = result.term.strip
      of tagDd:
        result.doc = e.innerText
        result.doc = result.doc.replace('\n', ' ')
        result.doc = result.doc.strip
        var nextChildIdx = 0
        e.tryParseMarginCodesBeginTag(nextChildIdx, result.codes)
        yield result
        result.codes.reset
      else: discard
    else: discard
