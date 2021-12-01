import htmlparser
import xmltree  # To use '$' for XmlNode
import strtabs  # To access XmlAttributes
import os       # To use splitFile
import strutils # To use cmpIgnoreCase
import pegs, sequtils, algorithm, options, tables

import scrape_common, doc_codes

let varPeg = """
var <- 'extern '? {type (' ' type)*} \s+ {ident} (', ' {ident})* ';'?
type <- \ident+ !(',' / $)
ident <- '*'* \ident+ '[]'?
""".peg

let funcPtrPeg = """
funcPtr <- 'extern '? {type} \s+ '(*' {ident} ')('({type} (',' \s* {type})*)? ')' ';'?
type <- ident ('*'+ / '[]')? (\s+ ident ('*'+ / '[]')?)?
ident <- \ident+
""".peg

# if "header shall define the timeval structure gg as desfcribed in " =~ peg"@ ('header shall de' ('fine' / 'clare') ' the ' {\w+} ' structure' (!' as described in' .)* !' as described in')":
  # echo "+"
# # echo "size_t  we_wordc, we_wordc".match(varPeg)
# # echo "size_t  we_wordc".match(varPeg)
# # echo "size_t we_wordc".match(varPeg)
# quit 0

let exportFromPeg = """
@ 'described in <' {(\w / '/' / '_')+} '.h>' @$
""".peg

let docCodePeg = """
code <- '[' {\w+} (' ' {\w+})*  ']'
""".peg

let headerNamePeg = """
line <- {@} \s '-' \s {@}$
""".peg

let headerSynopsisPeg = """
line <- code? {@} \s '-' \s {@}$ code?
code <- '['  ']'
""".peg

let namePeg = """
line <- funcs+ \s '-' \s summary
funcs <- func ', '?
func <- \w+
summary <- {@}$
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

# var body = loadHtml("./susv4-2018/functions/stat.html").child("html").child("body")
#let options = DefaultQueryOptions - { optSimpleNot }
#let elements = xml.querySelectorAll("p:not(.maybe-skip:nth-child(even))", options)

iterator next(n: XmlNode): XmlNode =
  for c in n:
    case c.kind:
    of xnElement: yield c
    else: discard

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
        r.add "#****h* posix/"
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

template basenames(glob: string): string =
  walkFiles(glob)

iterator functions: string =
  for f in basenames("./susv4-2018/functions/*.html"):
    yield f.splitFile.name

iterator basedefs: tuple[name: string, body: var XmlNode] =
  for f in basenames("./susv4-2018/basedefs/*.h.html"):
    yield (f.splitFile.name, loadHtml(f).child("html").child("body"))

template moduleName(cHeader: string): string =
  cHeader.splitFile.name

proc parseVars(line: string): seq[tuple[ident, kind: string]] =
  var r: tuple[ident, kind: string]
  var baseKind: string
  if line =~ varPeg:
    baseKind = matches[0]
    for match in matches[1..^1]:
      if match.len == 0: # matches is an array of strings
        break
      var off = 0
      var len = match.len-1
      if match[off] == '*':
        off.inc
        r.kind = "ptr "
        if match[off] == '*':
          off.inc
          r.kind.add "ptr "
      if match[^1] == ']':
        len.dec 2
        r.kind.add "ptr "
      r.kind.add baseKind
      r.ident.add match[off..len]
      result.add r
      r.reset
  elif line =~ funcPtrPeg:
    r.ident = matches[1]
    var retType = matches[0]
    var params: string
    for match in matches[2..^1]:
      if match.len > 0:
        params.add match & ','
    if params.len > 0:
      params.delete(params.len-1, params.len-1)
    r.kind = "proc(" & params & (if retType.len > 0: "): " & retType else: ")")

proc parseHeaderName(line: string): tuple[header, summary: string] =
  if line =~ headerNamePeg:
    return (matches[0], matches[1])

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

proc parseHeaderConsts(n: XmlNode): Table[string, tuple[desc: string, codes: set[Code], exportFrom: string]] =
  for e in n.betweenAll(("p", some "@ 'following symbolic constants'"), ("dl", string.none), ("p", string.none)):
    for def in e.defs:
      # some constants had '{' '}' around them
      let stripped = def.term.strip(chars = {'{', '}'})
      let exportFrom = def.desc.parseExportFrom
      result[stripped] = (def.desc, def.desc.parseCodes, exportFrom)

iterator vars*(n: XmlNode): tuple[ident, kind: string, codes: set[Code]] =
  let t = n.innerText.strip
  var r: tuple[ident, kind: string, codes: set[Code]]
  for line in t.splitLines:
    if line.len > 0:
      if r.codes.len == 0:
        if line[0] == '[': # line has start of codes
          r.codes = line.parseCodes
        else:
          for v in line.parseVars:
            r.ident = v.ident
            r.kind = v.kind
            yield r
      elif line[0] == '[': # line has end of codes
        r.codes.reset
      else:
        for v in line.parseVars:
          r.ident = v.ident
          r.kind = v.kind
          yield r

proc parseHeaderVars(n: XmlNode): Table[string, tuple[kind: string, codes: set[Code]]] =
  for e in n.betweenAll(("p", some r"@ ('following ' \w+ ' variables')"), ("pre > tt", string.none), ("p", string.none)):
    for v in e.vars:
      result[v.ident] = (v.kind, v.codes)

proc parseHeaderTypes(n: XmlNode): Table[string, tuple[desc: string, codes: set[Code], exportFrom: string]] =
  for e in n.betweenAll(("p", some r"@ ('header shall define' ' at least'? ' the following types:')"), ("dl", string.none), ("p", string.none)):
    for def in e.defs:
      # # some constants had '{' '}' around them
      # let stripped = def.term.strip(chars = {'{', '}'})
      let exportFrom = def.desc.parseExportFrom
      result[def.term] = (def.desc, def.desc.parseCodes, exportFrom)

proc parseHeaderStructs(n: XmlNode): Table[string, tuple[fields: Table[string, tuple[kind, desc: string, codes: set[Code]]], codes: set[Code]]] =
  for e in n:
    var captures: seq[string]
    var struct: Table[string, tuple[fields: Table[string, tuple[kind, desc: string, codes: set[Code]]], codes: set[Code]]]
    var defines: string
    var fieldIdent: string
    var field: tuple[kind, desc: string, codes: set[Code]]
    var fields: tuple[fields: Table[string, tuple[kind, desc: string, codes: set[Code]]], codes: set[Code]]
    var codes: set[Code]
    if e.matches(("p", some r"@ ('header shall de' ('fine' / 'clare') ' the ' {\w+} ' structure' (!' as described in' .)* !' as described in')"), captures):
      if e.innerText.contains("as described in"):
        echo e
      # var f = result.mgetOrPut(captures[0], fields)
      # var f: fields
      for e in n.betweenAll(("p", some r"@ ('header shall de' ('fine' / 'clare') ' the " & captures[0] & r" structure' (!' as described in' .)* !' as described in')"), ("pre > tt", string.none), ("p", string.none)):
        for c in e:
        #   var desc: string
          if c.kind == xnElement:
            if c.tag == "tt":
              for field in c.vars():
                fields.fields[field.ident] = (field.kind, "", {})
      result[captures[0]] = fields
        #       if c[0].kind == xnElement and c[0].tag == "sup": # a <sup> with codes
        #         codes = c[0].parseCodes
        #       elif c[0].kind == xnElement and c[0].tag == "img": # end of codes
        #         codes.reset
        #       let t = c.innerText.strip
        #       if t.len > 0:
        #         defines = t
        #       # echo defines
        #     elif c.tag == "i":
        #       desc.add " `" & c.innerText.replace('\n', ' ').strip & '`'
        #       echo desc
        #   elif c.kind == xnText:
        #     desc.add c.innerText.replace('\n', ' ').strip

template docLine(s: var string) =
  s.add "##\n"

template addLine(s: var string, t: string) =
  s.add t & '\n'

template docHeader(s: var string, t: string) =
  s.add "## " & t & '\n'

template doc(s: var string, t: string, indentLevel = 0) =
  s.add "##   " & indent(t, indentLevel) & '\n'

type Header = object
  moduleName: string
  filename: string
  doc: string
  consts: Table[string, tuple[desc: string, codes: set[Code], exportFrom: string]]
  vars: Table[string, tuple[kind: string, codes: set[Code]]]
  structs: Table[string, tuple[fields: Table[string, tuple[kind, desc: string, codes: set[Code]]], codes: set[Code]]]
  types: Table[string, tuple[desc: string, codes: set[Code], exportFrom: string]]
  funcs: seq[tuple[ident, desc: string, codes: set[Code]]]
  codes: set[Code]
  obMsg: string

proc parseHeader(header: var Header, body: var XmlNode) =
  transformOpts body
  var i = 0
  for _ in body.next i:
    if body[i].tag == "h4":
      var t = body[i].innerText
      case t:
      of "NAME":
        header.doc.addLine "#****h* posix/" & header.moduleName
        header.doc.docHeader t
        header.doc.docLine
        inc i
        for _ in body.next i:
          let r = parseHeaderName(body[i].innerText)
          header.filename = r.header
          header.doc.doc r.summary
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
        header.doc.docLine
        header.doc.docHeader t
        inc i
        for _ in body.next(i, xnElement):
          header.consts = body[i].parseHeaderConsts
          header.vars = body[i].parseHeaderVars
          header.types = body[i].parseHeaderTypes
          header.structs = body[i].parseHeaderStructs
          for line in body[i].innerText.splitLines:
            if line.strip.len > 0:
              header.doc.doc line
            else:
              header.doc.docLine
          inc i
          break
      else: discard
  header.doc.addLine "#******"

func pragmas(codes: set[Code]): string =
  var pragmas: Table[string, string]
  if Code.OB in codes:
    pragmas["deprecated"] = "deprecated in POSIX"
  if pragmas.len > 0:
    result = ", \"\"\"{."
    for (k, v) in pragmas.pairs():
      result.add k & ": \"" & v & "\", "
    result[^2..^1] = ".}"
    result.add "\"\"\""

func doc(doc: string): string =
  if doc.len > 0:
    return "\"\"\"" & doc & "\"\"\""
  else:
    return "TODO: document"

proc main =
  # var codes: string
  # for c in Code:
  #   codes.add $c & " / "
  # codes.delete(codes.len-4, codes.len-1)
  # echo codes
  removeDir "out"
  createDir "out"
  for f in basedefs():
    # for e in f.body.between(("h4:first-of-type", string.none), ("h4", some "SYNOPSIS")):
    #   echo e.innerText
    # for e in f.body.between(("h4", some "SYNOPSIS"), ("h4", some "DESCRIPTION")):
    #   var t = e.innerText
    #   t.stripLineEnd
    #   echo t.strip
    var module = Header(moduleName: moduleName(f.name))
    var mainModule = open("out" / module.moduleName & ".nim", fmWrite)

    parseHeader(module, f.body)
    mainModule.write module.doc
    if Code.OB in module.codes:
      if module.obMsg.len > 0:
        mainModule.writeLine "{.deprecated: \"" & module.obMsg & "\".}"
      else:
        mainModule.writeLine "{.deprecated: \"'" &
          module.filename & "' may be removed from future POSIX standard\".}"

    mainModule.writeLine "{.push header: \"" & module.filename & "\", importc.}"

    mainModule.writeLine "{.pop.}"

    mainModule.writeLine "import " & module.moduleName & "_impl"

    # constants
    if module.consts.len > 0:
      var genConstDefinesModule = open("out" / module.moduleName & "_consts_0.nim", fmWrite)
      genConstDefinesModule.writeLine "include ../ctypeof"
      let pushHeader = "{.push used, header: \"" & module.filename & "\".}"
      genConstDefinesModule.writeLine pushHeader
      genConstDefinesModule.writeLine "echo \"include ../ctypeof\""
      genConstDefinesModule.writeLine "echo \"\"\"" & pushHeader & "\"\"\""
      for (term, desc) in module.consts.pairs:
        genConstDefinesModule.writeLine "genDefine \"" & term & "\", " & doc(desc.desc) & pragmas(desc.codes)
      genConstDefinesModule.writeLine "echo \"{.pop.}\""
      genConstDefinesModule.writeLine "{.pop.}"
      genConstDefinesModule.close

    # vars
    if module.vars.len > 0:
      var varsModule = open("out" / module.moduleName & "_vars.nim", fmWrite)
      varsModule.writeLine "var"
      for (term, desc) in module.vars.pairs:
        varsModule.writeLine "  " & term &
          "* {.importc" & (if Code.OB in desc.codes: ", deprecated" else: "") & ".}: " & desc.kind
      varsModule.close
    # types
    if module.types.len > 0 or module.structs.len > 0:
      var typesModule = open("out" / module.moduleName & "_types.nim", fmWrite)
      typesModule.writeLine "{.push used, header: \"" & module.filename & "\".}"
      typesModule.writeLine "type"
      # typedefs
      for (term, desc) in module.types.pairs:
        typesModule.writeLine "  " & term &
          "* {.importc" & (if Code.OB in desc.codes: ", deprecated" else: "") & ".}: cint"
        typesModule.writeLine "    ## " & desc.desc
      # structs
      for (term, desc) in module.structs.pairs:
        typesModule.writeLine "  " & term &
          "* {.importc: \"struct " & term & "\"" & (if Code.OB in desc.codes: ", deprecated" else: "") & ".} = tuple"
        typesModule.writeLine "    ## TODO: document"
        for (fieldId, fieldVal) in desc.fields.pairs:
          typesModule.writeLine "    " & fieldId & pragmas(desc.codes) & ": " & fieldVal.kind
      typesModule.writeLine "{.pop.}"
      typesModule.close

    mainModule.close

  # for f in functions():
  #   echo f

main()
