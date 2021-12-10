
import pegs, options, tables

when isMainModule:
  import unittest
  import print

type
  Ctype* {.pure.} = enum
    Var, FuncPtr, Func
  CQaulifier* {.pure.} = enum
    Const, Volatile
  CPointer* {.pure.} = enum
    BarePtr = "*"
    ConstPtr = "*const"
    RestrictPtr = "*restrict"
  CDeclBase* = object
    qualifier*, primitive*, arrayDecl*: string
    pointers*: seq[CPointer]
  CDecl* = object
    ident*: string
    base*: CDeclBase
    case kind*: CType
    of Func, FuncPtr:
      params*: seq[CDecl]
      funcPointers*: seq[CPointer]
    else: discard


let varPeg = """
var <- 'extern '? {type (' ' type)*} \s+ {ident} (',' \s+ {ident})* ';'?
type <- \ident+ !(',' / '[' / $)
ident <- '*'* \ident+ ('[]' / '[' ([0-9]+ / \ident+) ']')?
""".peg

const
  typePegCommon = """
# funcPtrDecl <- funcPtrStart var
# funcDecl <-
# funcDeclStart <- type {ident} \s* '(' \s*
# funcPtrDeclStart <- type '(' pointers {ident}? ')(' ({type} (',' \s* {type})*)? ')' ';'?
# varDecl <- type ({ident} \s* {array}?)? \s*
params <- ({funcPtrDecl / varDecl} (\s* ',' \s* {funcPtrDecl / varDecl})*)?
funcDecl <- type funcDeclEnd
funcPtrDecl <- type funcPtrDeclEnd
varDecl <- type varDeclEnd?
funcDeclEnd <- {ident} \s* '(' \s* params \s* ')'
funcPtrDeclEnd <- '(' \s* pointers \s* {ident}? \s* ')' \s* '(' \s* params \s* ')'
varDeclEnd <- pointers? \s* {ident} \s* {array}?
type <- {linkage}? ({typeQualifier} \s+)? {primitive} \s* pointers? \s* {array}? \s*
typeQualifier <- 'const' / 'volatile'
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
typedef <- !keyword \ident+
keyword <- qualifier / primitive
qualifier <- 'const' / 'volatile' / 'restrict' / 'static'
primitive <- cbool / unsigned_int / signed_int / int / float / struct / union / void
struct <- 'struct' \s+ typedef
union <- 'union' \s+ typedef
unsigned_int <- cuchar / cushort / cuint / culonglong / culong
signed_int <- cschar / csshort / csint / cslonglong / cslong
int <- cchar / cshort / cint / clonglong / clong
cbool <- '_Bool'
cchar <- 'char'
cshort <- 'short'
cint <- 'int'
clong <- 'long'
cschar <- 'signed' \s+ 'char'
csshort <- 'signed' \s+ 'short'
csint <- 'signed' \s+ 'int'
cslonglong <- 'signed' \s+ clonglong
cslong <- 'signed' \s+ 'long'
cuchar <- 'unsigned' \s+ 'char'
cushort <- 'unsigned' \s+ 'short'
cuint <- 'unsigned' \s+ 'int'
culonglong <- 'unsigned' \s+ clonglong
culong <- 'unsigned' \s+ 'long'
clonglong <- 'long' \s+ 'long'
float <- cfloat / clongdouble / cdouble
cfloat <- 'float'
cdouble <- 'double'
clongdouble <- 'long' \s+ 'double'
void <- 'void'
"""

let declPeg = peg("""
t <- decl / declList
decl <- (funcPtrDecl / funcDecl / varDecl) \s* ';'? \s* $
declList <- type (funcPtrDeclEnd / funcDeclEnd / varDeclEnd) (\s* ',' \s* (funcPtrDeclEnd / funcDeclEnd / varDeclEnd))* \s* ';' \s* $
""" & typePegCommon)

func pop[T](s: var seq[T]): T {.discardable.} =
  result = system.pop(s)
  # print result

proc parse(s: string): seq[CDecl] =
  var
    decls: seq[CDecl]
    stack: seq[string]
    pointers: seq[CPointer]
    params: seq[CDecl]
    primitive: string
    ident: string
  let parse = declPeg.eventParser:
    pkNonTerminal:
      enter:
        # echo "? " & $start & ' ' & p.nt.name
        case p.nt.name
        of "funcPtrDecl":
          decls.add Cdecl(kind: FuncPtr)
        of "funcDecl":
          decls.add Cdecl(kind: Func)
        of "varDecl":
          decls.add Cdecl(kind: Var)
        of "declList":
          decls.add Cdecl()
        of "funcPtrDeclEnd":
          if stack.len > 0 and stack[0] == "declList":
            decls.add Cdecl(kind: FuncPtr)
            decls[^1].base = decls[0].base
        of "funcDeclEnd":
          if stack.len > 0 and stack[0] == "declList":
            decls.add Cdecl(kind: Func)
            decls[^1].base = decls[0].base
        of "varDeclEnd":
          if stack.len > 0 and stack[0] == "declList":
            decls.add Cdecl(kind: Var)
            decls[^1].base = decls[0].base
        stack.add p.nt.name
      leave:
        if length > 0:
          # echo "= " & $start & ' ' & p.nt.name
          let matchStr = s.substr(start, start+length-1)
          case p.nt.name
          of "typeQualifier":
            stack.pop
            decls[^1].base.qualifier = matchStr
          of "unsigned_int", "signed_int", "int", "float":
            stack.pop
          of "cbool", "cchar", "cshort", "cint", "clong", "cfloat", "cdouble",
             "cuchar", "cushort", "cuint", "culonglong", "culong",
             "cschar", "csshort", "csint", "cslonglong", "cslong",
             "clonglong", "clongdouble", "void":
            primitive = p.nt.name
            stack.pop
          of "struct", "union":
            primitive = p.nt.name & ' ' & ident
            stack.pop
          of "primitive":
            stack.pop
            decls[^1].base.primitive = primitive
            primitive.reset
          of "bare_pointer":
            stack.pop
            pointers.add BarePtr
          of "const_pointer":
            stack.pop
            pointers.add ConstPtr
          of "restrict_pointer":
            stack.pop
            pointers.add RestrictPtr
          of "pointer":
            stack.pop
            # pointers.add matchStr
          of "array":
            stack.pop
            decls[^1].base.arrayDecl = matchStr
          of "pointers":
            stack.pop
            if stack[^1] == "funcPtrDeclEnd":
              decls[^1].funcPointers = pointers
            else:
              decls[^1].base.pointers = pointers
            pointers.setLen 0
          of "ident":
            stack.pop
            decls[^1].ident = matchStr
          of "typedef":
            stack.pop
            ident = matchStr
          of "type":
            stack.pop
          of "funcDeclEnd", "funcPtrDeclEnd", "varDeclEnd":
            stack.pop
            # if stack.len > 0 and stack[0] == "declList":
            #   print decls[^1]
          of "funcPtrDecl":
            stack.pop
            if stack.len > 0 and stack[^1] == "params":
              let p = decls.pop
              decls[^1].params.add p
          of "funcDecl":
            stack.pop
          of "varDecl":
            stack.pop
            if stack.len > 0 and stack[^1] == "params":
              let p = decls.pop
              # print decls
              decls[^1].params.add p
          of "params":
            stack.pop
          of "declList":
            stack.pop
            decls = decls[1..^1]
            # decls.del(0) # TODO: this deletes the first element and then the last element is the first element???????
        else:
          # echo "! " & $start & ' ' & p.nt.name
          stack.pop
          # if "decl" == p.nt.name: print decls
          case p.nt.name
          of "decl", "funcPtrDecl", "funcDecl", "varDecl":
            if decls.len == 0: print stack
            decls.pop
          of "funcPtrDeclEnd", "funcDeclEnd", "varDeclEnd":
            if stack.len > 0 and stack[0] == "declList":
              decls.pop
  doAssert s.parse == s.len
  decls

import type_conv

proc toNimType(decl: var CDecl, exportAll = true, importC = true, isParam = false, distinctTypes: seq[(string, string, string)]): string =
  template vis(): untyped =
    if exportAll and not isParam: "*" else: ""
  func c(s: string): string =
    if s.len > 0:
      return s & ' '
    else: return ""
  func params(decl: var CDecl): string =
    result = "("
    var n = 0
    for p in decl.params.mitems:
      if n > 0:
        result.add ", "
      n.inc
      result.add p.toNimType(isParam = true, distinctTypes = distinctTypes)
    result &= ')'
  func `$`(ptrs: seq[CPointer]): string =
    for p in ptrs:
      result.add "ptr "

  var kind: string

  template returnType: untyped =
    if kind.len == 0 or kind == "void":
      ""
    else:
      ": " & kind

  # ptr void -> pointer
  if decl.base.pointers.len > 0 and decl.base.primitive == "void":
    decl.base.pointers.pop
    decl.base.primitive = "pointer"

  for ptrType in decl.base.pointers:
    kind.add "ptr "
  if decl.base.arrayDecl.len > 0:
    if decl.base.arrayDecl == "[]":
       # TODO: should this be an UncheckedArray?
      kind.add "ptr "

  if distinctTypes.len > 0:
    var hasRewrite = false
    for distinctType in distinctTypes:
      if decl.base.primitive == distinctType[0] and decl.ident == distinctType[1]:
        kind.add distinctType[2]
        hasRewrite = true
        break
    if not hasRewrite:
      kind.add cTypeToNimType(decl.base.primitive)
  else:
    kind.add cTypeToNimType(decl.base.primitive)

  case decl.kind
  of Var:
    if decl.ident.len > 0:
      if decl.base.qualifier == "const":
        result = (if isParam: "" else: "const ") & decl.ident & vis & ": "
      else:
        result = (if isParam: "" else: "var ") & decl.ident & vis & ": "
    result.add kind
  of Func:
    result = "proc " & decl.ident & vis & params(decl) & returnType
  of FuncPtr:
    result = (if isParam: "" else: "var ") & decl.ident & vis & ": " & $decl.funcPointers & "proc" & params(decl) & returnType

proc toNimType(decl: var CDecl, exportAll = true, importC = true, isParam = false): string =
  toNimType(decl, exportAll, importC, isParam, @[])

when isMainModule:
  suite "C Decl Conversion":
    test "primitives":
      var decls = parse("int")
      check decls[0].toNimType == "cint"
      decls = parse("unsigned int")
      check decls[0].toNimType == "cuint"
      decls = parse("unsigned int **")
      check decls[0].toNimType == "ptr ptr cuint"
      decls = parse("unsigned int ** a")
      check decls[0].toNimType == "var a*: ptr ptr cuint"
      decls = parse("unsigned int **[]")
      check decls[0].toNimType == "ptr ptr ptr cuint" # TODO: should this be: "ptr UncheckedArray[ptr ptr cuint]"
      decls = parse("int **restrict")
      check decls[0].toNimType == "ptr ptr cint"
      decls = parse("int *const*restrict")
      check decls[0].toNimType == "ptr ptr cint"
      decls = parse("int main()")
      check decls[0].toNimType == "proc main*(): cint"
      decls = parse("void* (**c)()")
      check decls[0].toNimType == "var c*: ptr ptr proc(): pointer"
      decls = parse("int main(int a)")
      check decls[0].toNimType == "proc main*(a: cint): cint"
      decls = parse("struct b* main(int a, void (*c)())")
      check decls[0].toNimType == "proc main*(a: cint, c: ptr proc()): ptr b"
      decls = parse("int main(int fd)")
      check decls[0].toNimType(distinctTypes = @[("cint", "fd", "FD")]) == "proc main*(fd: FD): cint"
      decls = parse("int a,b,*c;")
      check decls[0].toNimType == "var a*: cint"
      check decls[1].toNimType == "var b*: cint"
      check decls[2].toNimType == "var c*: ptr cint"

# func `$`(decl: CDeclBase): string =
#   func c(s: string): string =
#     if s.len > 0:
#       return s & ' '
#     else: return ""
#   result = decl.qualifier.c & decl.primitive.c
#   for ptrType in decl.pointers:
#     result.add $ptrType & ' '
#   if decl.arrayDecl.len > 0:
#     result.add decl.arrayDecl & ' '
#   return result[0..^2]
# when isMainModule:
#   suite "C Decl Parser":
#     test "base decl stringification":
#       var decls = parse("int")
#       check $decls[0].base == "int"
#       decls = parse("int **")
#       check $decls[0].base == "int * *"
#       decls = parse("int **restrict")
#       check $decls[0].base == "int * *restrict"
#       decls = parse("int **restrict []")
#       check $decls[0].base == "int * *restrict []"

# func `$`(decl: CDecl): string =
#   func c(s: string): string =
#     if s.len > 0:
#       return s & ' '
#     else: return ""
#   result = $decl.base
#   if decl.ident.len > 0:
#     result.add ' ' & decl.ident & ' '
#   # case decl.kind
#   # of Var: discard
#   # of Func:
#   #   decl.base.qualifier.c
#   # of FuncPtr:
#   #   decl.base.qualifier.c
#   return result[0..^2]

# when isMainModule:
#   suite "C Decl Parser":
#     test "func ptrs":
#       check:
#         parse("const long long (*aa)(int a, int (*b)(int n, struct s *restrict ss));")[0].kind == FuncPtr
#     test "funcs":
#       check:
#         parse("const long long aa(int a, int (*b)(int n, struct s *restrict ss));")[0].kind == Func
#     test "vars":
#       var decls = parse("int a")
#       check:
#         decls[0].kind == Var
#         $decls[0] == "int a"

#       decls = parse("int *a")
#       check:
#         decls[0].kind == Var
#         $decls[0] == "int * a"

# parse "int (*)()"
# parse "int (*a)(int a)"
# parse "int a, b,* ****c;"
# parse "int a"
# if "a b" =~ t: echo matches
# if "const char *, char *const [], char *const []" =~ t: echo matches
# if "int(*)()" =~ declPeg: echo matches
# if "int (*main)()" =~ funcPtrPeg: echo matches
# if "int main()" =~ funcPeg: echo matches
# if "volatile char *" =~ typePeg: echo matches
# if "volatile unsigned char *" =~ typePeg: echo matches
# if "volatile signed char *" =~ typePeg: echo matches
# if "extern volatile signed char *" =~ typePeg: echo matches
# if "const char*restrict" =~ typePeg: echo matches
# if "const char *" =~ typePeg: echo matches
# if "const char ** **" =~ typePeg: echo matches
# if "const char * const *" =~ typePeg: echo matches
# if "const char * const *const" =~ typePeg: echo matches
# if "const char * const * a;" =~ typePeg: echo matches
# if "const struct a_t * const * a;" =~ typePeg: echo matches
# if "const unsigned long long * const * a;" =~ typePeg: echo matches
# if "const char" =~ typePeg: echo matches
# if "const char[]" =~ typePeg: echo matches
# if "const char *" =~ typePeg: echo matches
# if "const char * a" =~ typePeg: echo matches
# if "const char * a[]" =~ typePeg: echo matches
# if "const char * a[8]" =~ typePeg: echo matches
# if "const char * a[hghghgf_hghgfhg]" =~ typePeg: echo matches
# if "const char * a[restrict]" =~ typePeg: echo matches
# let t = "(const char ************ a[restrict], const char * b[])"
# var matches: array[6,string]
# var bounds = t.findBounds(typePeg, matches, 0)
# echo matches
# echo bounds
# bounds = t.findBounds(typePeg, matches, bounds.last+1)
# echo matches
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
