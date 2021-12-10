import posix_common
export posix_common

const
  conformancePath* = "./posix_2018/basedefs/V1_chap02.html"
  subprofilesPath* = "./posix_2018/xrat/V4_subprofiles.html"
  codesPath* = "./posix_2018/help/codes.html"
  functionsGlob* = "./posix_2018/functions/*.html"
  headersGlob* = "./posix_2018/basedefs/*.h.html"

template headerPath*(name: string): string =
  "./posix_2018/basedefs/" & name & ".html"
