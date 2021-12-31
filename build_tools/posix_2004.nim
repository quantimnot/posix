import posix_common
export posix_common

const
  base = "./posix_2004"
  conformancePath* = base / "basedefs/xbd_chap02.html"
  subprofilesPath* = base / "xrat/subprofiles.html"
  codesPath* = base / "help/codes.html"
  functionsDir* = base / "functions"
  functionsGlob* = functionsDir / "*.html"
  errorCodesPath* = functionsDir / "xsh_chap02_03.html"
  headersGlob* = base / "basedefs/*.h.html"

template headerPath*(name: string): string =
  base / "basedefs/" & name & ".html"
