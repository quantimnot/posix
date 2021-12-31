import os       # To use splitFile
import options, tables, sequtils, strformat

import scrape_common, render_common, c_parser, type_conv, debug
import ../posix/options as posix_options, parse_codes, scrape_defs, ../posix/profiles, ../posix/errors

debug:
  import print

when defined posix_2004:
  import posix_2004
elif defined posix_2008:
  import posix_2008
elif defined posix_2013:
  import posix_2013
elif defined posix_2016:
  import posix_2016
else:
  {.define: posix_2018.}
  import posix_2018

type
  Var = tuple
    decl: CDecl
    # cType: string
    # nimType: string
    codes: set[Code]
    doc: string
  Consts = OrderedTable[string, tuple[doc: string, codes: set[Code], exportFrom: string]]
  Vars = OrderedTable[string, Var]
  Import = tuple
    ident: string
    codes: set[Code]
  Imports = tuple
    module: string
    symbols: seq[Import]
  Field = Var
  Fields = OrderedTable[string, Field]
  Struct = tuple
    ident: string
    fields: Fields
    codes: set[Code]
  Union = Struct
  # Arg = tuple
  #   decl: CDecl
  #   cType: string
  #   nimType: string
  Func = tuple
    decl: CDecl
    # ident: string
    # cType: string
    # nimType: string
    # args: OrderedTable[string, Arg]
    # hasOptionalArg: bool
    codes: set[Code]
    profile: Profile
    errorCodes: set[Error]
    doc: string
  Funcs = OrderedTable[string, Func]
  Structs = OrderedTable[string, Struct]
  Header = object
    moduleName: string
    filename: string
    doc: string
    consts: OrderedTable[string, tuple[doc: string, codes: set[Code], exportFrom: string]]
    vars: OrderedTable[string, tuple[kind: string, codes: set[Code]]]
    structs: OrderedTable[string, tuple[fields: OrderedTable[string, tuple[kind, cType, doc: string, codes: set[Code]]], codes: set[Code]]]
    types: OrderedTable[string, tuple[doc: string, codes: set[Code], exportFrom: string]]
    funcs: seq[tuple[ident, doc: string, codes: set[Code]]]
    codes: set[Code]
    imports: Table[string, seq[(string, set[Code])]]
    profile: string
    obMsg: string

# if "" =~ peg"'header shall de' ('fine' / 'clare') (!(\n / '<') .+)? '<b>' {\ident+} '</b>' ((!(\n / '<') .+)? '<b>' {\ident+} '</b>')* (!(\n / 'as described in') .+)? ' as described in'":
#   echo matches
# quit 0

let exportFromPeg = """
@ 'described in <' {(\w / '/' / '_')+} '.h>' @$
""".peg

let docCodePeg = """
code <- '[' {\w+} (' ' {\w+})*  ']'
""".peg

let headerNamePeg = """
line <- {@} \s '-' \s {@}('(' {[A-Z ]+} ')' / $)
""".peg

let headerSynopsisPeg = """
line <- code? {@} \s '-' \s {@}$ code?
code <- '['  ']'
""".peg

let namePeg = """
line <- funcs+ \s* '-' \s* summary profile?
funcs <- func (',' \s* func)*
func <- \ident+
summary <- [a-z ]+
profile <- \s* '(' profile_id ')'
profile_id <- [A-Z ]+
""".peg

let synopsisPeg = """
synopsis <- funcs  \s '-' \s summary
funcs <- func ', '?
func <- \w+
summary <- {@}$
""".peg

let parseSynopsis = synopsisPeg.eventParser:
  pkNonTerminal:
    leave:
      if length > 0:
        let matchStr = s.substr(start, start+length-1)
        case p.nt.name
        of "func": echo matchStr
        of "summary": echo matchStr

proc transformOpts(n: var XmlNode, opt: var string) =
  # var opt = ""
  for child in n.mitems:
    if child.kind == xnElement:
      if child.tag == "sup" and child.innerText[0] == '[':
        doAssert opt.len == 0, opt
        opt = child.innerText
        # continue
      elif opt.len > 0:
        if child.tag == "img" and child.attrs["src"] == "../images/opt-end.gif":
          child.add newText opt
          opt.reset
        else:
          transformOpts child, opt
      else:
        transformOpts child, opt

proc transformOpts(n: var XmlNode) =
  var opt = ""
  transformOpts n, opt

proc parseFunction(body: var XmlNode): string =
  transformOpts body
  var funcs: seq[string]
  let parseName = namePeg.eventParser:
    pkNonTerminal:
      leave:
        if length > 0:
          let matchStr = s.substr(start, start+length-1)
          case p.nt.name
          of "func":
            funcs.add matchStr
            echo matchStr
          of "summary": echo matchStr
  template r: untyped = result
  var i = 0
  for _ in body.next i:
    if body[i].tag == "h4":
      var t = body[i].innerText
      case t:
      of "NAME":
        r.add "## " & t
        inc i
        for _ in body.next i:
          discard parseName body[i].innerText
          inc i
          break
      # of "SYNOPSIS":
      #   inc i
      #   for _ in body.next i: break
      #   for p in body[i].next:
      #     for code in p.next:
      #       echo code.innerText
      #     break
      #   echo "## " & t
      else: discard

template moduleName(cHeader: string): string =
  cHeader.splitFile.name

proc parseVars(line: string, isParam = false, inclIdent = true): seq[Var] =
  var decls = line.parse
  for decl in decls:
    var r: Var
    r.decl = decl
    # r.cType = $decl
    # r.nimType = r.decl.toNimType(isParam = isParam, inclIdent = inclIdent)
    result.add r
  # try:
  #   var decls = line.parse
  #   if decls.len == 0:
  #     echo "no decls: " & line
  #     quit 1
  # except:
  #   echo "failed to parse: " & line
  #   quit 1
  # if line =~ varPeg:
  #   for match in matches[1..^1]:
  #     # these are the symbols with pointer and array decorations: ident, *ident1, ident2[], ident3[5]
  #     if match.len == 0:
  #       # matches is a fixed-length array of strings; an empty string marks terminates the last match
  #       break
  #     r.cType = # this is the base type; there can be more than one symbol per a line: basetype ident, *ident1, ident2[], ident3[5]
  #       if matches[0] == "unsigned": "unsigned int" # covers bug in spec; see basedefs/net_if.h.html
  #       else: matches[0]
  #     var symMatches: array[3, string]
  #     if match.match(decoratedSymbol, symMatches):
  #       r.ident = symMatches[1]
  #       if symMatches[0].len > 0:
  #         r.cType.add symMatches[0]
  #         if symMatches[0].len > 1:
  #           r.nimType = "ptr ptr " & matches[0].cTypeToNimType
  #         else:
  #           r.nimType = "ptr " & matches[0].cTypeToNimType
  #       if r.nimType.len == 0:
  #         r.nimType = r.cType.cTypeToNimType
  #       if symMatches[2].len > 0:
  #         if symMatches[2][0] == '[':
  #           r.cType.add symMatches[2]
  #           r.nimType = "UncheckedArray[" & r.nimType & ']'
  #         else:
  #           r.cType.add '[' & symMatches[2] & ']'
  #           r.nimType = "array[" & symMatches[2] & ',' & r.nimType & ']'
  #     else:
  #       raise newException(ParseError, "failed to parse: " & match)
  #     result.add r
  #     r.reset
  # elif line =~ funcPtrPeg:
  #   r.cType = matches[0] & ' '
  #   r.ident = matches[1]
  #   var retType = matches[0]
  #   var params: string
  #   for match in matches[2..^1]:
  #     if match.len > 0:
  #       params.add match & ','
  #   if params.len > 0:
  #     params.delete(params.len-1, params.len-1)
  #   r.nimType = "proc(" & params & (if retType.len > 0 and retType != "void": "): " & retType else: ")")
  #   result.add r
  # else:
  #   raise newException(ParseError, "unhandled c type: " & line)

proc parseHeaderName(line: string): tuple[header, summary, profile: string] =
  if line =~ headerNamePeg:
    return (matches[0], matches[1], matches[2])

proc parseExportFrom(line: string): string =
  if line =~ exportFromPeg:
    return matches[0]

proc parseCodes(s: string): set[Code] =
  if s =~ docCodePeg:
    var i = 0
    while i < matches.len:
      if matches[i].len > 0:
        result.incl parseEnum[Code](matches[i])
      i.inc

proc parseCodes(n: XmlNode, sel = "blockquote > div > tt > sup"): set[Code] =
  let e = n.querySelector(sel)
  if not e.isNil:
    return e.innerText.parseCodes

proc parseHeaderConsts(n: XmlNode): OrderedTable[string, tuple[doc: string, codes: set[Code], exportFrom: string]] =
  for e in n.betweenAll(("p", some "@ 'following symbolic constants'"), ("dl", string.none), ("p", string.none)):
    for def in e.defs:
      # some constants had '{' '}' around them
      let stripped = def.term.strip(chars = {'{', '}'})
      let exportFrom = def.doc.parseExportFrom
      result[stripped] = (def.doc, def.doc.parseCodes, exportFrom)

iterator vars*(n: XmlNode, isParam = false, inclIdent = true): Var =
  let t = n.innerText.strip
  var r: Var
  for line in t.splitLines:
    if line.len > 0:
      if r.codes.len == 0:
        if line[0] == '[': # line has start of codes
          r.codes = line.parseCodes
        else:
          for v in line.parseVars(isParam, inclIdent):
            r.decl = v.decl
            # r.nimType = v.nimType
            # r.cType = v.cType
            yield r
      elif line[0] == '[': # line has end of codes
        r.codes.reset
      else:
        for v in line.parseVars(isParam, inclIdent):
          r.decl = v.decl
          # r.nimType = v.nimType
          # r.cType = v.cType
          yield r

# proc parseHeaderVars(n: XmlNode): OrderedTable[string, tuple[kind: string, codes: set[Code]]] =
#   for e in n.betweenAll(("p", some r"@ ('following ' \w+ ' variables')"), ("pre > tt", string.none), ("p", string.none)):
#     for v in e.vars:
#       result[v.decl.ident] = (v.cType, v.codes)

proc parseHeaderTypes(n: XmlNode): OrderedTable[string, tuple[doc: string, codes: set[Code], exportFrom: string]] =
  for e in n.betweenAll(("p", some r"@ ('header shall define' ' at least'? ' the following types:')"), ("dl", string.none), ("p", string.none)):
    for def in e.defs:
      # # some constants had '{' '}' around them
      # let stripped = def.term.strip(chars = {'{', '}'})
      let exportFrom = def.doc.parseExportFrom
      result[def.term] = (def.doc, def.doc.parseCodes, exportFrom)

proc parseImports(n: XmlNode): Table[string, seq[(string, set[Code])]] =
  for e in n:
    var captures: array[1, string]
    if e.innerText.match(peg"@ 'header shall de' @ 'as described in' @ '<' {@} '>'", captures):
      result[captures[0]] = @[]
      for c in e.betweenAll(("", some r"'header shall de'"), ("b", string.none), ("", some r"'as described in'")):
        result[captures[0]].add (c.innerText, {})

# proc parseHeaderStructs(n: XmlNode): OrderedTable[string, tuple[fields: OrderedTable[string, tuple[kind, cType, doc: string, codes: set[Code]]], codes: set[Code]]] =
#   for e in n:
#     var captures: seq[string]
#     var struct: OrderedTable[string, tuple[fields: OrderedTable[string, tuple[kind, cType, doc: string, codes: set[Code]]], codes: set[Code]]]
#     var defines: string
#     var fieldIdent: string
#     # var field: tuple[kind, cType, doc: string, codes: set[Code]]
#     var fields: tuple[fields: OrderedTable[string, tuple[kind, cType, doc: string, codes: set[Code]]], codes: set[Code]]
#     var codes: set[Code]
#     if e.matches(("p", some r"@ ('header shall de' ('fine' / 'clare') ' the ' {\w+} ' structure' (!' as described in' .)* !' as described in')"), captures):
#       if e.innerText.contains("as described in"):
#         echo e
#       # var f = result.mgetOrPut(captures[0], fields)
#       # var f: fields
#       for e in n.betweenAll(("p", some r"@ ('header shall de' ('fine' / 'clare') ' the " & captures[0] & r" structure' (!' as described in' .)* !' as described in')"), ("pre > tt", string.none), ("p", string.none)):
#         var ident: string
#         for c in e:
#           if c.kind == xnElement:
#             if c.tag == "tt":
#               for field in c.vars(isParam = true, inclIdent = false):
#                 ident = field.decl.ident
#                 fields.fields[field.decl.ident] = (field.nimType, field.cType, "", {})
#           elif c.kind == xnText:
#             let text = c.text.strip
#             if text.len > 0 and ident.len > 0:
#               fields.fields[ident].doc = text
#               ident.reset
#       result[captures[0]] = fields
#         #       if c[0].kind == xnElement and c[0].tag == "sup": # a <sup> with codes
#         #         codes = c[0].parseCodes
#         #       elif c[0].kind == xnElement and c[0].tag == "img": # end of codes
#         #         codes.reset
#         #       let t = c.innerText.strip
#         #       if t.len > 0:
#         #         defines = t
#         #       # echo defines
#         #     elif c.tag == "i":
#         #       doc.add " `" & c.innerText.replace('\n', ' ').strip & '`'
#         #       echo doc
#         #   elif c.kind == xnText:
#         #     doc.add c.innerText.replace('\n', ' ').strip


proc tryParseHeaderConsts*(element: XmlNode, nextChildIdx: var int, consts: var Consts, codes: set[Code], doc: var string, bitwiseDistinct: var bool): bool =
  template e: untyped = element
  var i = nextChildIdx
  if e[i].htmlTag == tagP:
    if e[i].innerText.match(peg"@ 'following symbolic constant'"):
      bitwiseDistinct = e[i].innerText.match(peg"@ 'bitwise-distinct'")
      doc = e[i].innerText
      i.inc
      e.skipToMeaningfulElementOrText(i)
      if e[i].kind == xnElement and e[i].htmlTag == tagDl:
        doc.add e[i].innerText
        nextChildIdx = i
        result = true
        for def in e[i].defs:
          # some constants had '{' '}' around them # TODO: why?
          let stripped = def.term.strip(chars = {'{', '}'})
          let exportFrom = def.doc.parseExportFrom
          consts[stripped] = (def.doc, (if codes.len > 0: codes else: def.doc.parseCodes), exportFrom)


proc tryParseHeaderVars*(element: XmlNode, nextChildIdx: var int, vars: var Vars, codes: set[Code], doc: var string): bool =
  template e: untyped = element
  var i = nextChildIdx
  if e[i].htmlTag == tagP:
    if e[i].innerText.match(peg"@ ('following ' \w+ ' variable')"):
      doc = e[i].innerText
      i.inc
      e.skipToMeaningfulElementOrText(i)
      if e[i].kind == xnElement and e[i].htmlTag == tagPre:
        i.inc
        var n = 0
        e[i].skipToMeaningfulElementOrText(n)
        if e[i][n].kind == xnElement and e[i][n].htmlTag == tagTt:
          doc.add e[i].innerText
          nextChildIdx = i
          result = true
          for v in e[i].vars:
            vars[v.decl.ident] = v #(v.cType, v.codes)


proc tryParseHeaderTypedefs*(element: XmlNode, nextChildIdx: var int): bool =
  template e: untyped = element
  template i: untyped = nextChildIdx
  e[i].innerText.match(peg"@ ('shall define the following ' ('data types through typedef' / 'type'))")


proc tryParseHeaderInlineTypedefs*(element: XmlNode, nextChildIdx: var int): bool =
  template e: untyped = element
  template i: untyped = nextChildIdx
  e[i].innerText.match(peg"@ ('shall define the @ type')")


proc parseFields(element: XmlNode, nextChildIdx: var int, fields: var Fields, doc: var string): bool =
  template e: untyped = element
  var i = nextChildIdx
  doc = e[i].innerText
  i.inc
  e.skipToMeaningfulElementOrText(i)
  if e[i].kind == xnElement and e[i].htmlTag == tagPre:
    doc.add e[i].innerText
    nextChildIdx = i
    result = true
    var ident: string
    for c in e[i]:
      if c.kind == xnElement:
        if c.htmlTag == tagTt:
          for field in c.vars(isParam = true, inclIdent = false):
            ident = field.decl.ident
            fields[field.decl.ident] = field
      elif c.kind == xnText:
        let text = c.text.strip
        if text.len > 0 and ident.len > 0:
          fields[ident].doc = text
          ident.reset


proc tryParseHeaderStructs*(element: XmlNode, nextChildIdx: var int, struct: var Struct, doc: var string): bool =
  template e: untyped = element
  var i = nextChildIdx
  var ident: array[1, string]
  if e[i].htmlTag == tagP:
    if e[i].innerText.match(peg"@ ('header shall de' ('fine' / 'clare') ' the ' {\w+} ' structure' (!' as described in' .)* !' as described in')", ident):
      struct.ident = ident[0]
      result = parseFields(e, i, struct.fields, doc)
      nextChildIdx = i


proc tryParseHeaderUnions*(element: XmlNode, nextChildIdx: var int, union: var Union, doc: var string): bool =
  template e: untyped = element
  var i = nextChildIdx
  var ident: array[1, string]
  if e[i].htmlTag == tagP:
    if e[i].innerText.match(peg"@ ('header shall de' ('fine' / 'clare') ' the ' {\w+} ' union' (!' as described in' .)* !' as described in')", ident):
      union.ident = ident[0]
      result = parseFields(e, i, union.fields, doc)
      nextChildIdx = i


proc tryParseImports*(element: XmlNode, nextChildIdx: var int, imports: var Imports, doc: var string): bool =
  template e: untyped = element
  var i = nextChildIdx
  if e[i].htmlTag == tagP:
    # shall define the <b>clock_t</b>, <b>size_t</b>, <b>time_t</b>, types
    # shall define the <b>pid_t</b> type through <b>typedef</b>,
    # shall define the <b>struct timespec</b> structure
    # shall define the <b>sigevent</b> structure and <b>sigval</b> union
    var n = 0
    if n < e[i].len and e[i][n].kind == xnText:
      n.inc
      e[i].skipToMeaningfulElementOrText(n)
      if n < e[i].len and e[i][n].kind == xnElement and e[i][n].htmlTag == tagI:
        n.inc
        e[i].skipToMeaningfulElementOrText(n)
        if n < e[i].len and e[i][n].kind == xnText and e[i][n].innerText.contains("shall de"):
          while n < e[i].len and not (e[i][n].kind == xnElement and e[i][n].htmlTag == tagA):
            if e[i][n].kind == xnText:
              if e[i][n].innerText =~ peg"(@ {[A-Z_]+})+":
                for match in matches:
                  if match.len > 0:
                    imports.symbols.add (match, {})
            elif e[i][n].kind == xnElement:
              if e[i][n].htmlTag == tagB and not e[i][n].innerText.contains("typedef"):
                imports.symbols.add (e[i][n].innerText, {})
              elif e[i][n].htmlTag == tagI:
                imports.symbols.add (e[i][n].innerText, {})
            n.inc
            e[i].skipToMeaningfulElementOrText(n)
          if n < e[i].len and e[i][n].kind == xnElement and e[i][n].htmlTag == tagA:
            var match: array[1, string]
            if e[i][n].innerText.match(peg"@ '<' {@} '>'", match):
              nextChildIdx = i
              result = true
              imports.module = match[0].replace('/', '_').splitFile.name
              doc = e[i].innerText
          else: echo $e[i][n]


proc parseFuncNames(text: string): tuple[idents: seq[string], summary: string, profile: Profile] =
  var r: tuple[idents: seq[string], summary: string, profile: Profile]
  let parseName = namePeg.eventParser:
    pkNonTerminal:
      leave:
        if length > 0:
          template match: string = s.substr(start, start+length-1)
          case p.nt.name
          of "func":
            r.idents.add match()
          of "summary":
            r.summary = match()
          of "profile_id":
            r.profile = parseEnum[Profile](match())
  doAssert text.parseName == text.len
  r


proc parseFuncFile(path: string): tuple[doc: string, funcs: Funcs, vars: Vars] =
  # TODO:
  #   - [ ] There are some variables that need handled: posix_2018/functions/exec.html
  var body = loadHtml(path).child("html").child("body")
  var i = 0
  var funcNames: tuple[idents: seq[string], summary: string, profile: Profile]
  var text: string
  for _ in body.next i:
    if body[i].tag == "h4":
      var t = body[i].innerText
      case t:
      of "NAME":
        inc i
        for _ in body.next i:
          text = body[i].innerText
          funcNames = text.parseFuncNames
          # result.idents = funcNames.idents
          result.doc.comment t
          result.doc.commentLine
          result.doc.comment text, docIndentLevel = 2
          result.doc.commentLine
          inc i
          break
      of "SYNOPSIS":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        # result.doc.comment t
        # result.doc.comment text, docIndentLevel = 2
        for line in text.splitLines:
          if line.isEmptyOrWhitespace:
            continue
          var decls = line.strip.parse
          for decl in decls.mitems:
            case decl.kind
            of CType.Func: result.funcs[decl.ident] = (decl, {}, funcNames.profile, {}, "")
            of CType.Var: result.vars[decl.ident] = (decl, {}, "")
            else: doAssert false
      of "DESCRIPTION":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        result.doc.comment t
        result.doc.comment text, docIndentLevel = 2
      of "RETURN VALUE":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        result.doc.comment t
        result.doc.comment text, docIndentLevel = 2
      of "ERRORS":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        result.doc.comment t
        result.doc.comment text, docIndentLevel = 2
      of "APPLICATION USAGE":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        result.doc.comment t
        result.doc.comment text, docIndentLevel = 2
      of "RATIONALE":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        result.doc.comment t
        result.doc.comment text, docIndentLevel = 2
      of "FUTURE DIRECTIONS":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        result.doc.comment t
        result.doc.comment text, docIndentLevel = 2
      of "SEE ALSO":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        result.doc.comment t
        result.doc.comment text, docIndentLevel = 2
      of "CHANGE HISTORY":
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].kind == xnElement
        text = body[i].innerText
        result.doc.comment t
        result.doc.comment text, docIndentLevel = 2


proc tryParseFuncProtos*(element: XmlNode, nextChildIdx: var int, funcs: var seq[tuple[ident: string, codes: set[Code]]], doc: var string): bool =
  template e: untyped = element
  var i = nextChildIdx
  if e[i].htmlTag == tagP:
    if e[i].innerText.match(peg"@ 'following shall be declared as' ' a'? ' function'"):
      doc = e[i].innerText
      i.inc
      e.skipToMeaningfulElementOrText(i)
      if e[i].kind == xnElement and e[i].htmlTag == tagPre:
        var n = 0
        e[i].skipToMeaningfulElementOrText(n)
        if e[i][n].kind == xnElement and e[i][n].htmlTag == tagTt:
          doc.add e[i][n].innerText
          nextChildIdx = i
          result = true
          let e = e[i][n]
          n = 0
          var codes: set[Code]
          while n < e.len:
            if e[n].kind == xnText:
              var ident: array[1, string]
              for line in e[n].innerText.splitLines:
                if scrape_common.isEmptyOrWhitespace(line):
                  continue
                if line.match(peg"@ '  ' {\ident+} '('", ident):
                  funcs.add (ident[0], codes)
              n.inc
              e.skipToMeaningfulElementOrText(n)
            elif e.tryParseMarginCodesBeginTag(n, codes):
              continue
            elif e.tryParseMarginCodesEndTag(n):
              codes.reset
            else: doAssert false, $e[n]


proc tryMayMakeVisible*(element: XmlNode, nextChildIdx: var int): bool =
  template e: untyped = element
  template i: untyped = nextChildIdx
  # may make visible all symbols
  e[i].innerText.match(peg"@ 'make visible all symbols'")


proc parseHeader(header: var Header, body: var XmlNode) =
  transformOpts body
  var i = 0
  var funcSyms: seq[tuple[ident: string, codes: set[Code]]]
  var codes: set[Code]
  var cx: CExtension
  var doc, parsedDoc: string
  var indentLvl = 0
  var blockIndentLvl = 0

  for _ in body.next i:
    if body[i].tag == "h4":
      var t = body[i].innerText
      case t:
      of "NAME":
        when defined renderSrcDoc:
          # doc.addLine "#****h* posix/" & header.moduleName
          doc.commentHeader t
          doc.commentLine
        inc i
        for _ in body.next i:
          let r = parseHeaderName(body[i].innerText)
          header.filename = r.header
          when defined renderFfiCompileCheck:
            let pushStmt = """{.push importc, header: \"""" & header.filename & """\".}"""
            doc.addLine indent("""
when defined posixGenFfi:
  include ../build_tools/ctypeof
  echo "include ../build_tools/ctypeof"
  echo """" & pushStmt & """"
  echo "echo \"\"\"""" & pushStmt & """\"\"\""""", indentLvl)
            indentLvl.inc 2
            doc.addLine indent("{.push importc, header: \"" & header.filename & "\".}", indentLvl)
            indentLvl.dec 2
          when defined renderSrcDoc:
            doc.commentLine
            doc.comment r.summary, docIndentLevel = 2
          header.profile = r.profile
          inc i
          break
      of "SYNOPSIS":
        inc i
        for _ in body.next i:
          header.codes = parseCodes(body[i])
          # if not isEmptyOrWhitespace($body[i]):
          #   echo body[i].innerText
          inc i
          break
      of "DESCRIPTION":
        when defined renderSrcDoc:
          doc.commentLine
          doc.commentHeader t
        inc i
        body.skipToMeaningfulElementOrText(i)
        doAssert body[i].htmlTag == tagBlockquote
        # maybe I should change this to something like:
        var nextChildIdx = 0
        body[i].skipToMeaningfulElementOrText(nextChildIdx)
        cx = body[i][nextChildIdx].parseCx(nextChildIdx)
        body[i].skipToMeaningfulElementOrText(nextChildIdx)
        while nextChildIdx < body[i].len:
          # if an element is successfully parsed, then `nextChildIdx` is set to the next element index
          # if body[i][nextChildIdx].htmlTag in {tagBr, tagH5, tagHr, tagBasefont}:
          #   nextChildIdx.inc
          # echo body[i][nextChildIdx].kind
          # echo body[i][nextChildIdx].innerText
          case body[i][nextChildIdx].htmlTag
          of tagP:
            if codes.len == 0:
              var n = 0
              body[i][nextChildIdx].tryParseMarginCodesBeginTag(n, codes)
          else: discard
          #   if body[i].classifyParagraph(nextChildIdx) == ParagraphKind.Unknown:
          #     echo header.moduleName
          #     echo body[i][nextChildIdx]
          # of tagDl: discard
          # of tagUl: discard
          # of tagPre: discard
          # of tagBlockquote: discard
          # of tagCenter: discard
          # of tagTable: discard
          # of tagBr, tagH5, tagHr, tagBasefont: discard # these can be ignored
          # of tagImg: discard
          # else: doAssert false, body[i][nextChildIdx].tag & "  " & header.moduleName
          # is the element margin codes?
          # the start of a multiline set of elements inside margin code markers [CODES] ... [CODES]
          if codes.len > 0 and body[i].tryParseMarginCodesEndTag(nextChildIdx):
            nextChildIdx.inc
            body[i].skipToMeaningfulElementOrText(nextChildIdx)
            codes.reset
            continue

          var consts: Consts
          var bitwiseDistinct = false
          if body[i].tryParseHeaderConsts(nextChildIdx, consts, codes, parsedDoc, bitwiseDistinct):
            when defined renderSrcDoc:
              doc.commentLine
              doc.comment parsedDoc, docIndentLevel = 2
            when defined renderImpl:
              when defined renderFfiCompileCheck:
                doc.addLine indent("when defined posixImpl:", indentLvl)
                blockIndentLvl.inc 2
                indentLvl.inc blockIndentLvl
              # if codes.len > 0:
              #   for code in codes:
              #     echo indent("when defined has" & $code.codeToOption & ':', indentLvl)
              #     blockIndentLvl.inc 2
              #     indentLvl.inc 2

              # gen values
              var v = 0
              if bitwiseDistinct:
                v = 1
              for (ident, c) in consts.pairs:
                let lastIndentLvl = indentLvl
                if c.codes.len > 0:
                  for code in c.codes:
                    doc.addLine indent("when defined has" & $code.codeToOption & ':', indentLvl)
                    indentLvl.inc 2
                doc.addLine indent("const " & ident & "* = " & $v, indentLvl)
                doc.doc c.doc, indentLevel = indentLvl + 2, docIndentLevel = 0
                if bitwiseDistinct:
                  v = v shl 1
                else:
                  v.inc
                indentLvl = lastIndentLvl
              indentLvl.dec blockIndentLvl
              blockIndentLvl = 0

            proc `$`(opts: set[PosixOption]): string =
              result = "{"
              for opt in opts:
                result.add $opt & ", "
              if opts.len > 0:
                result = result[0..^3]
              result.add '}'

            when defined renderFfiCompileCheck:
              doc.addLine indent("when defined posixGenFfi:", indentLvl)
              indentLvl.inc 2
              for (ident, c) in consts.pairs:
                let text = "genDefine \"" & ident & '"' &
                ", \"\"\"" & c.doc & "\"\"\"" &
                ", pragmas = \"\"\"" & c.codes.deprecatedPragma & "\"\"\""
                doc.addLine indent(text, indentLvl)
              indentLvl.dec 2
              nextChildIdx.inc
              body[i].skipToMeaningfulElementOrText(nextChildIdx)
              continue

          var struct: Struct
          if body[i].tryParseHeaderStructs(nextChildIdx, struct, parsedDoc):
            when defined renderSrcDoc:
              doc.commentLine
              doc.comment parsedDoc, docIndentLevel = 2
            when defined renderFfi:
              doc.addLine indent("when defined posixFfi:", indentLvl)
              let lastIndentLvl = indentLvl
              indentLvl.inc 2
              doc.addLine indent("type " & struct.ident & '*' & pragmas(struct.codes, header = header.filename, importc = "struct " & struct.ident) & " = tuple", indentLvl)
              doc.comment "TODO: document", indentLevel = indentLvl + 2, docIndentLevel = 2
              for (fieldId, fieldVal) in struct.fields.pairs:
                doc.addLine indent(fieldId & deprecatedPragma(struct.codes) & ": " & fieldVal.decl.toNimType(isParam = true, inclIdent = false), indentLvl + 2)
                if fieldVal.doc.len > 0:
                  doc.doc fieldVal.doc, indentLevel = indentLvl + 4, docIndentLevel = 0
                # echo "    ## C type: " & fieldVal.cType
              indentLvl = lastIndentLvl
            nextChildIdx.inc
            body[i].skipToMeaningfulElementOrText(nextChildIdx)
            continue

          when defined renderImports:
            var imports: Imports
            if body[i].tryParseImports(nextChildIdx, imports, parsedDoc):
              when defined renderSrcDoc:
                doc.commentLine
                doc.comment parsedDoc, docIndentLevel = 2
              let lastIndentLvl = indentLvl
              when defined renderFfiCompileCheck:
                doc.addLine indent("when not defined posixGenFfi:", indentLvl)
                indentLvl.inc 2
              doc.addLine indent("when not defined nimdoc:", indentLvl)
              indentLvl.inc 2
              for symbol in imports.symbols:
                if symbol.codes.len > 0:
                  for code in symbol.codes:
                    doc.addLine indent("when defined has" & $code.codeToOption & ':', indentLvl)
                    indentLvl.inc 2
                doc.addLine indent("from " & imports.module & " import " & symbol.ident, indentLvl)
              indentLvl = lastIndentLvl
              nextChildIdx.inc
              body[i].skipToMeaningfulElementOrText(nextChildIdx)
              continue

          if body[i].tryParseFuncProtos(nextChildIdx, funcSyms, parsedDoc):
            # parseFuncFile(functionsDir / ident[0] & ".html")
            when defined renderSrcDoc:
              doc.commentLine
              doc.comment parsedDoc, docIndentLevel = 2
            # for symbol in imports.symbols:
            #   let lastIndentLvl = indentLvl
            #   if symbol.codes.len > 0:
            #     for code in symbol.codes:
            #       echo indent("when defined has" & $code.codeToOption & ':', indentLvl)
            #       indentLvl.inc 2
            #   echo indent("from " & imports.module & " import " & symbol.ident, indentLvl)
            nextChildIdx.inc
            body[i].skipToMeaningfulElementOrText(nextChildIdx)
            continue

          var vars: Vars
          if body[i].tryParseHeaderVars(nextChildIdx, vars, codes, parsedDoc):
            nextChildIdx.inc
            body[i].skipToMeaningfulElementOrText(nextChildIdx)
            continue

          when defined renderSrcDoc:
            doc.commentLine
            doc.comment body[i][nextChildIdx].innerText, docIndentLevel = 2
          #    tryParseHeaderTypes(header, body, nextChildIdx, codes) or
          #    tryParseHeaderStructs(header, body, nextChildIdx, codes) or
          #    tryParseImports(header, body, nextChildIdx, codes): continue
          nextChildIdx.inc
          body[i].skipToMeaningfulElementOrText(nextChildIdx)
        # for _ in body.next(i, xnElement):
        #   header.consts = body[i].parseHeaderConsts
        #   header.vars = body[i].parseHeaderVars
        #   header.types = body[i].parseHeaderTypes
        #   header.structs = body[i].parseHeaderStructs
        #   header.imports = body[i].parseImports
        #   # print header.imports
        #   for line in body[i].innerText.splitLines:
        #     if line.strip.len > 0:
        #       doc.doc line
        #     else:
        #       doc.docLine
        #   inc i
        #   break
      else: discard
  # doc.addLine "#******"
  # var funcs: OrderedTable[string, tuple[html: XmlNode, summary: string, codes: set[Code], profile: Profile, parsedAlongWith: seq[string]]]
  var parsed: seq[string]
  if funcSyms.len > 0:
    for funcSym in funcSyms:
      if not (funcSym.ident in parsed):
        var decls = parseFuncFile(functionsDir / funcSym.ident & ".html")
        parsed.add decls.funcs.keys.toSeq
        when defined renderSrcDoc:
          doc.commentLine
          doc.addLine decls.doc
        let lastIndentLvl = indentLvl
        # when defined renderFfiCompileCheck:
        #   doc.addLine indent("when not defined posixGenFfi:", indentLvl)
        #   indentLvl.inc 2
        when defined renderFfi:
          doc.addLine indent("when defined posixFfi:", indentLvl)
          indentLvl.inc 2
          for fn in decls.funcs.values:
              doc.addLine indent(fn.decl.toNimType & pragmas(codes, header.filename), indentLvl)
              doc.addLine ""
        indentLvl = lastIndentLvl
        # var html = loadHtml(functionsDir / funcSym.ident & ".html").child("html").child("body")
        # for n in html.between(("h4", some "NAME"), ("blockquote", none string), ("h4", some "SYNOPSIS")):
        #   var idents: seq[string]
        #   var summary: string
        #   var profile: Profile
        #   let parseName = namePeg.eventParser:
        #     pkNonTerminal:
        #       leave:
        #         if length > 0:
        #           template match: string = s.substr(start, start+length-1)
        #           case p.nt.name
        #           of "func":
        #             idents.add match()
        #           of "summary":
        #             summary = match()
        #           of "profile_id":
        #             profile = parseEnum[Profile](match())
        #   let text = n.innerText
        #   doAssert text.parseName == text.len
        #   if not funcs.hasKey(idents[0]):
        #     funcs[idents[0]] = (html, summary, funcSym.codes, profile, idents[1..^1])
        #   echo funcs.keys.toSeq
          # for ident in idents:
          #   if not funcs.hasKey(ident):
          #     funcs[ident] = (html, summary, funcSym.codes, profile, idents[0])
          #     echo idents[0] & " " & ident
  when defined renderFfiCompileCheck:
    doc.addLine indent("when defined posixGenFfi:", indentLvl)
    indentLvl.inc 2
    doc.addLine indent("{.pop.}", indentLvl)
    doc.addLine indent("echo \"{.pop.}\"", indentLvl)
    doc.addLine indent("""echo "echo \"{.pop.}\""""", indentLvl)
  echo doc

const
  ffiDir {.strdefine.} = "posix/ffi"

proc main =
  # var codes: string
  # for c in Code:
  #   codes.add $c & " / "
  # codes.delete(codes.len-4, codes.len-1)
  # echo codes
  removeDir ffiDir
  createDir ffiDir
  for f in basedefs("./posix_2018/basedefs/fcntl.h.html"): #headersGlob):
    # for e in f.body.between(("h4:first-of-type", string.none), ("h4", some "SYNOPSIS")):
    #   echo e.innerText
    # for e in f.body.between(("h4", some "SYNOPSIS"), ("h4", some "DESCRIPTION")):
    #   var t = e.innerText
    #   t.stripLineEnd
    #   echo t.strip
    var module = Header(moduleName: moduleName(f.name))

    parseHeader(module, f.body)
    # section mainModule:
    #   write module.doc
    #   if Code.OB in module.codes:
    #     if module.obMsg.len > 0:
    #       writeLine "{.deprecated: \"" & module.obMsg & "\".}"
    #     else:
    #       writeLine "{.deprecated: \"'" &
    #         module.filename & "' may be removed from future POSIX standard\".}"
    #   if module.profile.len > 0:
    #     echo module.profile
    #   writeLine "{.push header: \"" & module.filename & "\", importc.}"
    #   writeLine "{.pop.}"
    #   writeLine "import " & module.moduleName & "_impl"

    # if module.consts.len > 0:
    #   section consts, "_consts_0":
    #     writeLine "include ../ctypeof"
    #     let pushHeader = "{.push used, header: \"" & module.filename & "\".}"
    #     writeLine pushHeader
    #     writeLine "echo \"include ../ctypeof\""
    #     writeLine "echo \"\"\"" & pushHeader & "\"\"\""
    #     for (term, doc) in module.consts.pairs:
    #       writeLine "genDefine \"" & term & "\", " & doc(doc.doc) & pragmas(doc.codes, header.filename)
    #     writeLine "echo \"{.pop.}\""
    #     writeLine "{.pop.}"

    # if module.vars.len > 0:
    #   section vars, "_vars":
    #     writeLine "var"
    #     for (term, doc) in module.vars.pairs:
    #       writeLine "  " & term &
    #         "* {.importc" & (if Code.OB in doc.codes: ", deprecated" else: "") & ".}: " & doc.kind

    # if module.types.len > 0 or module.structs.len > 0:
    #   section types, "_types":
    #     writeLine "{.push used, header: \"" & module.filename & "\".}"
    #     writeLine "type"
    #     # typedefs
    #     for (term, doc) in module.types.pairs:
    #       writeLine "  " & term &
    #         "* {.importc" & (if Code.OB in doc.codes: ", deprecated" else: "") & ".}: cint"
    #       writeLine "    ## " & doc.doc
    #     # structs
    #     for (term, doc) in module.structs.pairs:
    #       writeLine "  " & term &
    #         "* {.importc: \"struct " & term & "\"" & (if Code.OB in doc.codes: ", deprecated" else: "") & ".} = tuple"
    #       writeLine "    ## TODO: document"
    #       for (fieldId, fieldVal) in doc.fields.pairs:
    #         writeLine "    " & fieldId & pragmas(doc.codes) & ": " & fieldVal.kind
    #         if fieldVal.doc.len > 0:
    #           writeLine "      ## " & fieldVal.doc
    #         writeLine "      ## C type: " & fieldVal.cType
    #     writeLine "{.pop.}"

  # for f in functions():
  #   echo f

main()
