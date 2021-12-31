import posix_common
export posix_common

const
  base = "./posix_2008"
  conformancePath* = base / "basedefs/V1_chap02.html"
  subprofilesPath* = base / "xrat/V4_subprofiles.html"
  codesPath* = base / "help/codes.html"
  functionsDir* = base / "functions"
  functionsGlob* = functionsDir / "*.html"
  errorCodesPath* = functionsDir / "V2_chap02.html"
  headersGlob* = base / "basedefs/*.h.html"

template headerPath*(name: string): string =
  base / "basedefs/" & name & ".html"
