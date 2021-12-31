import tables, strutils, ../posix/codes, ../posix/options

func pragmas*(codes: set[Code], header: string, importc = ""): string =
  var pragmas: Table[string, string]
  pragmas["importc"] = importc
  pragmas["header"] = header
  if Code.OB in codes:
    pragmas["deprecated"] = "deprecated in POSIX"
  if pragmas.len > 0:
    result = " {."
    for (k, v) in pragmas.pairs():
      if v.len > 0:
        result.add k & ": \"" & v & "\", "
      else:
        result.add k & ", "
    result[^2..^1] = ".}"


template deprecatedPragma*(codes: set[Code]): string =
  if Code.OB in codes: " {.deprecated: \"deprecated in POSIX\".}" else: ""


func codesToOptions*(codes: set[Code]): set[PosixOption] =
  for code in (codes - {Code.OB}):
    result.incl code.codeToOption


# func doc*(doc: string): string =
#   if doc.len > 0:
#     return "\"\"\"" & doc & "\"\"\""
#   else:
#     return "TODO: document"


template section*(name, fileSuffix, code): untyped =
  block:
    var name = open(ffiDir / module.moduleName & fileSuffix & ".nim", fmWrite)
    # var posix = open(ffiDir / "posix_all.nim", fmWrite)
    template write(s): untyped =
      name.write s
      # posix.write s
    template writeLine(s): untyped =
      name.writeLine s
      # posix.writeLine s
    code
    name.close
    # posix.close


template writeDoc*(doc) =
  echo ""
  for line in doc.splitLines:
    if not line.isEmptyOrWhitespace:
      echo indent("# " & line, indentLvl)


template section*(name, code): untyped =
  section(name, "", code)


template addLine*(s: var string, t: string) =
  s.add t & '\n'


template docLine*(s: var string) =
  s.add "##\n"


template commentLine*(s: var string) =
  s.add "#\n"


template docHeader*(s: var string, t: string) =
  s.add "## " & t & '\n'


template commentHeader*(s: var string, t: string) =
  s.add "# " & t & '\n'


template comment*(s: var string, t = "", indentLevel = 0, docIndentLevel = 0, marker = "#") =
  var blankLineCount = 0
  for line in t.splitLines:
    if line.isEmptyOrWhitespace:
      if blankLineCount == 0:
        s.addLine repeat(' ', indentLevel) & marker
      blankLineCount.inc
    else:
      s.addLine repeat(' ', indentLevel) & marker & ' ' & repeat(' ', docIndentLevel) & line
      blankLineCount = 0


template doc*(s: var string, t: string, indentLevel = 0, docIndentLevel = 2) =
  comment(s, t, indentLevel, docIndentLevel, marker = "##")


func doc*(t: string, indentLevel = 0): string =
  for line in t.splitLines:
    if not line.isEmptyOrWhitespace:
      result.addLine "## " & repeat("  ", indentLevel) & line


template printGeneratedByComment*(source: string, commentLeader = "#") =
  echo commentLeader & " GENERATED from the \"" & source & "\" page of the POSIX html spec by `" & instantiationInfo().filename & '`'


template render*(code): untyped =
  block:
    template writeLine(s): untyped =
      stdout.writeLine s
    code


# template render*(name, code): untyped =
#   render(name, "", code)
