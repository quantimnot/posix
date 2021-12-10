import pegs

type NimTypeCType* {.pure.} = enum
  unknown
  # bool = "_Bool"
  # char
  # byte = "unsigned char"
  # int8 = "int64_t"
  # int16 = "int16_t"
  # int32 = "int32_t"
  # int64 = "int64_t"
  # int = "int"
  # float32 = "float"
  # float64 = "double"
  # float = "double"
  # uint8 = "uint8_t"
  # uint16 = "uint16_t"
  # uint32 = "uint32_t"
  # uint64 = "uint64_t"
  # uint = "unsigned int"
  # pchar = "ptr char"
  # pint = "ptr int"

  bool = "_Bool"
  cchar = "char"
  cschar = "signed char"
  cuchar = "unsigned char"
  cshort = "short"
  cushort = "unsigned short"
  cint = "int"
  cuint = "unsigned int"
  clong = "long"
  culong = "unsigned long"
  clonglong = "long long"
  culonglong = "unsigned long long"
  cfloat = "float"
  cdouble = "double"
  clongdouble = "long double"
  cstring = "char*"
  cstringArray = "char**"
  void = "void"
  pointer = "void*"
  # `tuple` = "struct"
  # union = "union"

let structOrUnion = peg"('struct' / 'union') \s {\ident+}{'*'+}?"
let typedef = peg"{\ident+}{'*'+}?"

proc cTypeToNimType*(t: string): string =
  case t
  of "_Bool": return "bool"
  of "char": return "cchar"
  of "signed char": return "cschar"
  of "unsigned char": return "cuchar"
  of "short": return "cshort"
  of "unsigned short": return "cushort"
  of "int": return "cint"
  of "unsigned int": return "cuint"
  of "long": return "clong"
  of "unsigned long": return "culong"
  of "long long": return "clonglong"
  of "unsigned long long": return "culonglong"
  of "float": return "cfloat"
  of "double": return "cdouble"
  of "long double": return "clongdouble"
  of "char*": return "cstring"
  of "char**": return "cstringArray"
  of "void*": return "pointer"
  of "void": return "void"
  elif t =~ structOrUnion:
    case matches[1].len
    of 0: return matches[0]
    of 1: return "ptr " & matches[0]
    of 2: return "ptr ptr " & matches[0] # TODO: should this be `ptr UncheckedArray[type]`
    else: discard
  elif t =~ typedef:
    return t
  else:
    raise newException(ValueError, "unhandle type: " & t)

when isMainModule:
  import unittest
  suite "C to Nim type conversion":
    test "primitives":
      check:
        cTypeToNimType("int") == "cint"
    test "typedefs":
      check:
        cTypeToNimType("size_t") == "size_t"
    test "structs & unions":
      check:
        cTypeToNimType("struct a_t") == "a_t"
        cTypeToNimType("struct a_t*") == "ptr a_t"
        cTypeToNimType("struct a_t**") == "ptr ptr a_t"
        cTypeToNimType("union a_t") == "a_t"
        cTypeToNimType("union a_t*") == "ptr a_t"
        cTypeToNimType("union a_t**") == "ptr ptr a_t"
    test "arrays":
      check:
        cTypeToNimType("uint8_t s6_addr[a]") == "array[a, uint8_t]"
        cTypeToNimType("uint8_t* s6_addr[6]") == "array[6, ptr uint8_t]"
        cTypeToNimType("uint8_t s6_addr[]") == "ptr UncheckedArray[uint8_t]"
        cTypeToNimType("uint8_t* s6_addr[]") == "ptr UncheckedArray[ptr uint8_t]"
