const maxLinwWidth {.intdefine.} = 100

type NimPrimitives {.pure.} = enum
  unknown
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
  default: """ & $NimPrimitives.unknown.ord & """)
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
    {.emit: "int " & `fns` & "() { return typename(" & `s` & "); }".}
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

template genDefine*(ident: string, doc, pragmas = "") =
  var result: string
  var kind = $ctypeof(ident)
  result = "define \"" & ident & "\", " & kind & ", \"\"\"" & doc & "\"\"\""
  echo result

template declare*(ident: string, doc = "") =
  var result: string
  var kind = $ctypeof(ident)
  result = "  " & ident & "* {.importc, nodecl.}: " & kind
  if doc.len > 0:
    result.add "\n    ## " & doc
  echo result

template define*(ident: string; kind; doc, pragmas = "") =
  var result: string
  var value = $cvalue(ident, kind)
  result = "  " & $ident & (if pragmas.len > 0: " {." & pragmas & ".} " else: " ") & "= " & $kind & '(' & value & ')'
  if doc.len > 0:
    result.add "\n    ## " & doc
  echo result

# {.push header: "unistd.h".}
# echo "const"
# let X_OK {.importc, nodecl.}: int
# declare "X_OK"
# define "X_OK", int
# define "X_OK", cint, ""
# genDefine "X_OK"
# # echo define("X_OK")
# {.pop.}
