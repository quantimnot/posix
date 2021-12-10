import htmlparser
import xmltree  # To use '$' for XmlNode
import strtabs  # To access XmlAttributes
import os       # To use splitFile
import strutils # To use cmpIgnoreCase
import pegs, sequtils, algorithm, options, tables

import scrape_common, type_conv, ../posix/codes

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
  CQaulifier {.pure.} = enum
    Const, Volatile, Restrict
  Var = tuple
    ident: string
    cType: string
    nimType: string
    cQualifiers: seq[CQaulifier]
    codes: set[Code]
  Header = object
    moduleName: string
    filename: string
    doc: string
    consts: OrderedTable[string, tuple[desc: string, codes: set[Code], exportFrom: string]]
    vars: OrderedTable[string, tuple[kind: string, codes: set[Code]]]
    structs: OrderedTable[string, tuple[fields: OrderedTable[string, tuple[kind, cType, desc: string, codes: set[Code]]], codes: set[Code]]]
    types: OrderedTable[string, tuple[desc: string, codes: set[Code], exportFrom: string]]
    funcs: seq[tuple[ident, desc: string, codes: set[Code]]]
    codes: set[Code]
    profile: string
    obMsg: string

let decoratedSymbol = peg"{'*'*} {\ident+} ({'[]'} / '[' {[0-9]+ / \ident+} ']')?"

let varPeg = """
var <- 'extern '? {type (' ' type)*} \s+ {ident} (',' \s+ {ident})* ';'?
type <- \ident+ !(',' / '[' / $)
ident <- '*'* \ident+ ('[]' / '[' ([0-9]+ / \ident+) ']')?
""".peg

const
  typePegCommon = """
type <- {linkage}? ({'const' / 'volatile'} \s+)? {primitive} \s* pointers? \s* {array}? \s*
linkage <- 'extern' \s+
pointers <- {pointer} (\s* {pointer})*
pointer <- const_pointer / restrict_pointer / bare_pointer
bare_pointer <- '*'
const_pointer <- '*' \s* 'const'
restrict_pointer <- '*' \s* 'restrict'
array <- array_pointer / restrict_array / sized_array
restrict_array <- '[' \s* 'restrict' \s* ']'
sized_array <- '[' \s* ([0-9]+ / ident)? \s* ']'
array_pointer <- '[' \s* ']'
ident <- !keyword \ident+
keyword <- qualifier / primitive
qualifier <- 'const' / 'volatile' / 'restrict' / 'static'
primitive <- '_Bool' / unsigned_int / signed_int / int / float / struct / union
struct <- 'struct' \s+ ident
union <- 'union' \s+ ident
unsigned_int <- uchar / ushort / uint / ulonglong / ulong
signed_int <- schar / sshort / sint / slonglong / slong
int <- 'char' / 'short' / 'int' / longlong / 'long'
schar <- 'signed' \s+ 'char'
sshort <- 'signed' \s+ 'short'
sint <- 'signed' \s+ 'int'
slonglong <- 'signed' \s+ longlong
slong <- 'signed' \s+ 'long'
uchar <- 'unsigned' \s+ 'char'
ushort <- 'unsigned' \s+ 'short'
uint <- 'unsigned' \s+ 'int'
ulonglong <- 'unsigned' \s+ longlong
ulong <- 'unsigned' \s+ 'long'
longlong <- 'long' \s+ 'long'
float <- 'float' / longdouble / 'double'
longdouble <- 'long' \s+ 'double'
"""

let typePeg = peg("""
decl <- type ({ident} \s* {array}?)? \s*
""" & typePegCommon)

let funcPeg = peg("""
funcDeclStart <- type {ident} \s* '(' \s*
""" & typePegCommon)

let funcPtrPeg = peg("""
funcPtrStart <- type '(' pointers {ident}? ')(' ({type} (',' \s* {type})*)? ')' ';'?
""" & typePegCommon)

# if "a b" =~ t: echo matches
# if "const char *, char *const [], char *const []" =~ t: echo matches
if "int(*)()" =~ funcPtrPeg: echo matches
if "int (*main)()" =~ funcPtrPeg: echo matches
if "int main()" =~ funcPeg: echo matches
if "volatile char *" =~ typePeg: echo matches
if "volatile unsigned char *" =~ typePeg: echo matches
if "volatile signed char *" =~ typePeg: echo matches
if "extern volatile signed char *" =~ typePeg: echo matches
if "const char*restrict" =~ typePeg: echo matches
if "const char *" =~ typePeg: echo matches
if "const char ** **" =~ typePeg: echo matches
if "const char * const *" =~ typePeg: echo matches
if "const char * const *const" =~ typePeg: echo matches
if "const char * const * a;" =~ typePeg: echo matches
if "const struct a_t * const * a;" =~ typePeg: echo matches
if "const unsigned long long * const * a;" =~ typePeg: echo matches
if "const char" =~ typePeg: echo matches
if "const char[]" =~ typePeg: echo matches
if "const char *" =~ typePeg: echo matches
if "const char * a" =~ typePeg: echo matches
if "const char * a[]" =~ typePeg: echo matches
if "const char * a[8]" =~ typePeg: echo matches
if "const char * a[hghghgf_hghgfhg]" =~ typePeg: echo matches
if "const char * a[restrict]" =~ typePeg: echo matches
let t = "(const char ************ a[restrict], const char * b[])"
var matches: array[6,string]
var bounds = t.findBounds(typePeg, matches, 0)
echo matches
echo bounds
bounds = t.findBounds(typePeg, matches, bounds.last+1)
echo matches
# if "const * char *[]" =~ t: echo matches

# # echo "size_t  we_wordc, we_wordc".match(varPeg)
# # echo "size_t  we_wordc".match(varPeg)
# if "int             aio_fildes" =~ varPeg: echo matches
# echo "uint8_t s6_addr[3]".match(varPeg)
# if "**s6_addr[a]" =~ decoratedSymbol: echo matches
# if "*s6_addr[a]" =~ decoratedSymbol: echo matches
# if "s6_addr[]" =~ decoratedSymbol: echo matches
# if "s6_addr[a]" =~ decoratedSymbol: echo matches
# if "void   (*sa_sigaction)(const char *, char *const [], char *const [])" =~ funcPtrPeg: echo matches
# if "void   (*sa_sigaction)(int, siginfo_t*, void *)" =~ funcPtrPeg: echo matches
# if "void   (*sa_sigaction)(int, siginfo_t)" =~ funcPtrPeg: echo matches
# if "void   (*sa_sigaction)(int*)" =~ funcPtrPeg: echo matches
# if "void   (*sa_sigaction)(int)" =~ funcPtrPeg: echo matches
# if "a void   (*sa_sigaction)(int)" =~ funcPtrPeg: echo matches
# if "void           (*sigev_notify_function)(union sigval)" =~ funcPtrPeg: echo matches
quit 0

proc parse*(parser: GdbMiParser, resp: string): Option[Output] =
  ## TODO
  ##   - [ ] cleanup
  var
    possible: seq[string]
    match: string
    tupleDepth: int
    listDepth: int
    listVal: List
    valLists: seq[seq[Value]]
    resLists: seq[seq[Result]]
    tupleVal: Tuple
    value: Value
    res: Result
    resultPairs: seq[Result]
    results: seq[Result]
    token: Token
    resRec: ResultRec
    o: Output
    r: Option[Output]
  let miParser = parser.peg.eventParser:
    pkNonTerminal:
      enter:
        debug "? " & p.nt.name
        case p.nt.name
        of "tuple":
          possible.add p.nt.name
          tupleDepth.inc
          # debug "tupleDepth start " & $tupleDepth
        of "value_list":
          valLists.add @[]
          possible.add p.nt.name
          listDepth.inc
          # debug "listDepth start " & $listDepth
        of "empty_list":
          possible.add p.nt.name
        of "result_list":
          resLists.add @[]
          possible.add p.nt.name
          listDepth.inc
        of "list":
          listDepth.inc
          # possible.add "list"
        of "result":
          possible.add p.nt.name
      leave:
        if length > 0:
          match = s.substr(start, start+length-1)
          debug "= " & p.nt.name
          case p.nt.name
          of "token":
            token = match.Token
          of "const":
            value.`const` = match.strip(chars = {'"'})
          of "tuple":
            possible.delete(possible.len-1)
            # debug "tupleDepth finish " & $tupleDepth
            debug $results.len
            debug $results
            if tupleDepth > 0:
              tupleDepth.dec
              value.kind = ValueKind.Tuple
              value.`tuple` = tupleVal
              tupleVal.clear
              debug "tupleVal clear"
          of "list":
            # possible.delete(possible.len-1)
            debug $possible
            # NOTE
            #   A list is either empty, contains only values, or contains only results (pairs).
            debug "listDepth finish " & $listDepth
            # if listDepth > 0:
            #   listDepth.dec
            #   value.kind = ValueKind.List
            #   value.`list` = listVal
            #   # listVal.clear
            #   debug "listVal clear"
            # value.`list` = listVal
          of "value_list":
            possible.delete(possible.len-1)
            if listDepth > 0:
              listDepth.dec
              value.kind = ValueKind.ValueList
              value.values = valLists.pop
              # listVal.clear
              debug "listVal clear"
            # value.`list` = listVal
          of "result_list":
            possible.delete(possible.len-1)
            if listDepth > 0:
              listDepth.dec
              value.kind = ValueKind.ResultList
              value.results = resLists.pop
              # listVal.clear
              debug "listVal clear"
            # value.`list` = listVal
          of "variable":
            resultPairs.add Result(key: match)
            debug $possible
            debug $resultPairs
          of "value":
            debug $possible
            case possible[^1]
            of "value_list":
              debug "list value"
              valLists[^1].add value
              debug $valLists
            of "result_list":
              debug "list result"
              resLists[^1].add res
              debug $resLists
            of "empty_list":
              resultPairs[^1].val.kind = ValueKind.ValueList
              resultPairs[^1].val.values = @[]
              debug $resultPairs
            else:
              debug "result value"
              resultPairs[^1].val = value
              debug $resultPairs
            value.reset
            debug "value reset"
          of "result":
            debug $possible
            let v = resultPairs.pop
            if possible.len > 1:
              possible.delete(possible.len-1)
            case possible[^1]
            of "result_list":
              debug "list result"
              resLists[^1].add v
              debug $resLists
              debug $resultPairs
            of "tuple":
              debug "tuple result"
              tupleVal[v.key] = v.val
            else:
              results.add v
            # if tupleDepth > 0:
            #   tupleVal[v.key] = v.val
            # else:
            #   results.add v
            # resultPair.reset
            # debug "resultPair reset"
          of "result_class":
            case match
            of $ResultKind.Done:
              resRec.kind = ResultKind.Done
            of $ResultKind.Running:
              # For the reason this sets `Done` instead of `Running`,
              # see https://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Result-Records.html#GDB_002fMI-Result-Records
              resRec.kind = ResultKind.Done
            of $ResultKind.Connected:
              resRec.kind = ResultKind.Connected
            of $ResultKind.Error:
              resRec.kind = ResultKind.Error
            of $ResultKind.Exit:
              resRec.kind = ResultKind.Exit
            else:
              fatal "unhandled result kind"
          of "result_record":
            resRec.results = results
            o.res = some resRec
            resRec.reset
            debug "resRec reset"
          of "output":
            r = some o
        else:
          debug "! " & p.nt.name
          case p.nt.name
          of "tuple":
            tupleDepth.dec
            possible.delete(possible.len-1)
          of "list":
            listDepth.dec
            # possible.delete(possible.len-1)
          of "value_list", "result_list", "empty_list":
            possible.delete(possible.len-1)

  debug "\n\nParse:\n" & resp
  let parsedLen = miParser(resp) # TODO: do something with this result
  debug $r
  r

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

template moduleName(cHeader: string): string =
  cHeader.splitFile.name

proc parseVars(line: string): seq[Var] =
  var r: Var
  if line =~ varPeg:
    for match in matches[1..^1]:
      # these are the symbols with pointer and array decorations: ident, *ident1, ident2[], ident3[5]
      if match.len == 0:
        # matches is a fixed-length array of strings; an empty string marks terminates the last match
        break
      r.cType = # this is the base type; there can be more than one symbol per a line: basetype ident, *ident1, ident2[], ident3[5]
        if matches[0] == "unsigned": "unsigned int" # covers bug in spec; see basedefs/net_if.h.html
        else: matches[0]
      var symMatches: array[3, string]
      if match.match(decoratedSymbol, symMatches):
        r.ident = symMatches[1]
        if symMatches[0].len > 0:
          r.cType.add symMatches[0]
          if symMatches[0].len > 1:
            r.nimType = "ptr ptr " & matches[0].cTypeToNimType
          else:
            r.nimType = "ptr " & matches[0].cTypeToNimType
        if r.nimType.len == 0:
          r.nimType = r.cType.cTypeToNimType
        if symMatches[2].len > 0:
          if symMatches[2][0] == '[':
            r.cType.add symMatches[2]
            r.nimType = "UncheckedArray[" & r.nimType & ']'
          else:
            r.cType.add '[' & symMatches[2] & ']'
            r.nimType = "array[" & symMatches[2] & ',' & r.nimType & ']'
      else:
        raise newException(ParseError, "failed to parse: " & match)
      result.add r
      r.reset
  elif line =~ funcPtrPeg:
    r.cType = matches[0] & ' '
    r.ident = matches[1]
    var retType = matches[0]
    var params: string
    for match in matches[2..^1]:
      if match.len > 0:
        params.add match & ','
    if params.len > 0:
      params.delete(params.len-1, params.len-1)
    r.nimType = "proc(" & params & (if retType.len > 0 and retType != "void": "): " & retType else: ")")
    result.add r
  else:
    raise newException(ParseError, "unhandled c type: " & line)

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

proc parseHeaderConsts(n: XmlNode): OrderedTable[string, tuple[desc: string, codes: set[Code], exportFrom: string]] =
  for e in n.betweenAll(("p", some "@ 'following symbolic constants'"), ("dl", string.none), ("p", string.none)):
    for def in e.defs:
      # some constants had '{' '}' around them
      let stripped = def.term.strip(chars = {'{', '}'})
      let exportFrom = def.desc.parseExportFrom
      result[stripped] = (def.desc, def.desc.parseCodes, exportFrom)

iterator vars*(n: XmlNode): Var =
  let t = n.innerText.strip
  var r: Var
  for line in t.splitLines:
    if line.len > 0:
      if r.codes.len == 0:
        if line[0] == '[': # line has start of codes
          r.codes = line.parseCodes
        else:
          for v in line.parseVars:
            r.ident = v.ident
            r.nimType = v.nimType
            r.cType = v.cType
            yield r
      elif line[0] == '[': # line has end of codes
        r.codes.reset
      else:
        for v in line.parseVars:
          r.ident = v.ident
          r.nimType = v.nimType
          r.cType = v.cType
          yield r

proc parseHeaderVars(n: XmlNode): OrderedTable[string, tuple[kind: string, codes: set[Code]]] =
  for e in n.betweenAll(("p", some r"@ ('following ' \w+ ' variables')"), ("pre > tt", string.none), ("p", string.none)):
    for v in e.vars:
      result[v.ident] = (v.cType, v.codes)

proc parseHeaderTypes(n: XmlNode): OrderedTable[string, tuple[desc: string, codes: set[Code], exportFrom: string]] =
  for e in n.betweenAll(("p", some r"@ ('header shall define' ' at least'? ' the following types:')"), ("dl", string.none), ("p", string.none)):
    for def in e.defs:
      # # some constants had '{' '}' around them
      # let stripped = def.term.strip(chars = {'{', '}'})
      let exportFrom = def.desc.parseExportFrom
      result[def.term] = (def.desc, def.desc.parseCodes, exportFrom)

proc parseHeaderStructs(n: XmlNode): OrderedTable[string, tuple[fields: OrderedTable[string, tuple[kind, cType, desc: string, codes: set[Code]]], codes: set[Code]]] =
  for e in n:
    var captures: seq[string]
    var struct: OrderedTable[string, tuple[fields: OrderedTable[string, tuple[kind, cType, desc: string, codes: set[Code]]], codes: set[Code]]]
    var defines: string
    var fieldIdent: string
    # var field: tuple[kind, cType, desc: string, codes: set[Code]]
    var fields: tuple[fields: OrderedTable[string, tuple[kind, cType, desc: string, codes: set[Code]]], codes: set[Code]]
    var codes: set[Code]
    if e.matches(("p", some r"@ ('header shall de' ('fine' / 'clare') ' the ' {\w+} ' structure' (!' as described in' .)* !' as described in')"), captures):
      if e.innerText.contains("as described in"):
        echo e
      # var f = result.mgetOrPut(captures[0], fields)
      # var f: fields
      for e in n.betweenAll(("p", some r"@ ('header shall de' ('fine' / 'clare') ' the " & captures[0] & r" structure' (!' as described in' .)* !' as described in')"), ("pre > tt", string.none), ("p", string.none)):
        var ident: string
        for c in e:
          if c.kind == xnElement:
            if c.tag == "tt":
              for field in c.vars():
                ident = field.ident
                fields.fields[field.ident] = (field.nimType, field.cType, "", {})
          elif c.kind == xnText:
            let text = c.text.strip
            if text.len > 0 and ident.len > 0:
              fields.fields[ident].desc = text
              ident.reset
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
  for f in basedefs(headersGlob):
    # for e in f.body.between(("h4:first-of-type", string.none), ("h4", some "SYNOPSIS")):
    #   echo e.innerText
    # for e in f.body.between(("h4", some "SYNOPSIS"), ("h4", some "DESCRIPTION")):
    #   var t = e.innerText
    #   t.stripLineEnd
    #   echo t.strip
    var module = Header(moduleName: moduleName(f.name))
    var mainModule = open(ffiDir / module.moduleName & ".nim", fmWrite)
    parseHeader(module, f.body)
    mainModule.write module.doc
    if Code.OB in module.codes:
      if module.obMsg.len > 0:
        mainModule.writeLine "{.deprecated: \"" & module.obMsg & "\".}"
      else:
        mainModule.writeLine "{.deprecated: \"'" &
          module.filename & "' may be removed from future POSIX standard\".}"
    if module.profile.len > 0:
      echo module.profile
    mainModule.writeLine "{.push header: \"" & module.filename & "\", importc.}"
    mainModule.writeLine "{.pop.}"
    mainModule.writeLine "import " & module.moduleName & "_impl"
    mainModule.close

    # constants
    if module.consts.len > 0:
      var genConstDefinesModule = open(ffiDir / module.moduleName & "_consts_0.nim", fmWrite)
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
      var varsModule = open(ffiDir / module.moduleName & "_vars.nim", fmWrite)
      varsModule.writeLine "var"
      for (term, desc) in module.vars.pairs:
        varsModule.writeLine "  " & term &
          "* {.importc" & (if Code.OB in desc.codes: ", deprecated" else: "") & ".}: " & desc.kind
      varsModule.close
    # types
    if module.types.len > 0 or module.structs.len > 0:
      var typesModule = open(ffiDir / module.moduleName & "_types.nim", fmWrite)
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
          if fieldVal.desc.len > 0:
            typesModule.writeLine "      ## " & fieldVal.desc
          typesModule.writeLine "      ## C type: " & fieldVal.cType
      typesModule.writeLine "{.pop.}"
      typesModule.close

  # for f in functions():
  #   echo f

main()
