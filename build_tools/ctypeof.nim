import strutils
import ../posix/options

const maxLinwWidth {.intdefine.} = 100

type NimPrimitives {.pure.} = enum
  undefined
  bool
  char
  byte
  int8
  int16
  int32
  int64
  int
  float32
  float64
  float
  uint8
  uint16
  uint32
  uint64
  uint
  pchar = "ptr char"
  pint = "ptr int"
  pointer
  cchar
  cschar
  cuchar
  cshort
  cushort
  cint
  cuint
  clong
  culong
  clonglong
  culonglong

{.emit: """
#define typename(x) _Generic((x), \
  _Bool: """ & $NimPrimitives.bool.ord & """, \
  char: """ & $NimPrimitives.cchar.ord & """, \
  signed char: """ & $NimPrimitives.cschar.ord & """, \
  unsigned char: """ & $NimPrimitives.cuchar.ord & """, \
  short int: """ & $NimPrimitives.cshort.ord & """, \
  unsigned short int: """ & $NimPrimitives.cushort.ord & """, \
  int: """ & $NimPrimitives.cint.ord & """, \
  unsigned int: """ & $NimPrimitives.cuint.ord & """, \
  long int: """ & $NimPrimitives.clong.ord & """, \
  unsigned long int: """ & $NimPrimitives.culong.ord & """, \
  long long int: """ & $NimPrimitives.clonglong.ord & """, \
  unsigned long long int: """ & $NimPrimitives.culonglong.ord & """, \
  float: """ & $NimPrimitives.float32.ord & """, \
  double: """ & $NimPrimitives.float64.ord & """, \
  long double: """ & $NimPrimitives.float64.ord & """, \
  char *: """ & $NimPrimitives.pchar.ord & """, \
  void *: """ & $NimPrimitives.pointer.ord & """, \
  int *: """ & $NimPrimitives.pint.ord & """, \
  default: """ & $NimPrimitives.undefined.ord & """)
"""
.}

import macros

macro ctypeof*(ident: string): untyped =
  result = newStmtList()
  let s = $ident
  let fn = ident("typeof_" & s)
  let fns = $fn
  result.add quote do:
    {.emit: "#define _POSIX_C_SOURCE 200809L".}
    {.emit: "#define _XOPEN_SOURCE 700".}
    {.emit: """
int """ & `fns` & """() {
#ifdef """ & `s` & '\n' & """
  return typename(""" & `s` & """);
#else
  return """ & $NimPrimitives.undefined.ord & """;
#endif
}""".}
  result.add quote do:
    proc `fn`: cint {.importc, nodecl.}
  result.add quote do:
    NimPrimitives(`fn`())
  # echo repr result

macro cvalue*(ident, kind): untyped =
  result = newStmtList()
  let s = ident($ident)
  # let fn = ident("valueof_" & s)
  # let fns = $fn
  # let v = "valueof_" & s
  # let vi = ident v
  # let vig = ident "get" & v
  result.add quote do:
    let `s` {.importc, nodecl.}: `kind`
    `s`
    # proc `vig`: int = cast[int](`vi`)
    # {.emit: "void * " & `v` & "() { return (void *) &" & `s` & "; }".}
  # echo repr result
  # result.add quote do:
  #   let `s` {.importc, nodecl.}: `t`
  #   echo $`s`

template genDefine*(ident: string, doc = "", options: set[PosixOption] = {}, pragmas = "") =
  var result: string
  var kind = ctypeof(ident)
  var indentLvl = 0
  if options.len > 0:
    for option in options:
      result.add indent("when defined has" & $option & ":\n", indentLvl)
      indentLvl.inc 2
  if kind == undefined:
    result.add indent("""{.warning: "undefined POSIX symbol: `""" & ident & """`".}""", indentLvl)
  else:
    result.add indent("define \"" & ident & "\", " & $kind, indentLvl)
    if doc.len > 0:
      result.add ", \"\"\"" & doc & "\"\"\""
    if pragmas.len > 0:
      result.add ", \"\"\"" & pragmas & "\"\"\""
  echo result

template declare*(ident: string, doc = "") =
  var result: string
  var kind = $ctypeof(ident)
  result = "  " & ident & "* {.importc, nodecl.}: " & kind
  if doc.len > 0:
    result.add "\n    ## " & doc
  echo result

template define*(ident: string; kind; doc, pragmas = "", doImport = true) =
  var result: string
  var value = $cvalue(ident, kind)
  if doImport:
    result = "let "
  else:
    result = "const "
  result.add $ident & '*'
  if pragmas.len > 0:
    result.add pragmas
  result.add " = " & $kind & '(' & value & ')'
  if doc.len > 0:
    result.add "\n  ## " & doc
  echo result

# {.push header: "fcntl.h".}
# genDefine "DOESNOTEXIST", options = {PosixAdvisoryInfo}
# genDefine "F_DUPFD_CLOEXEC", """Duplicate file descriptor with the close-on- exec flag FD_CLOEXEC set."""
# genDefine "F_GETFD", """Get file descriptor flags."""
# genDefine "F_SETFD", """Set file descriptor flags."""
# genDefine "F_GETFL", """Get file status flags and file access modes."""
# genDefine "F_SETFL", """Set file status flags."""
# genDefine "F_GETLK", """Get record locking information."""
# genDefine "F_SETLK", """Set record locking information."""
# genDefine "F_SETLKW", """Set record locking information; wait if blocked."""
# genDefine "F_GETOWN", """Get process or process group ID to receive SIGURG signals."""
# genDefine "F_SETOWN", """Set process or process group ID to receive SIGURG signals."""
# {.pop.}
