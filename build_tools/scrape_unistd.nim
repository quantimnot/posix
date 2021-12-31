#****bt* posix/scrape_profiles
## PURPOSE
##   This program scrapes the POSIX conformance profile constants from
##   `unistd.h` and writes a c program to stdout.
##   The c program is platform agnostic. Its purpose is to be executed
##   on each supported platform and its stdout parsed for compatibility.
## USAGE
##   ```
##   nim r scrape_profiles.nim > conformance.c
##   # compile and run for each supported platform
##   cc -w conformance.c -o conformance
##   ./conformance > conformance.lst
##   ```
#******
import scrape_common, scrape_defs, render_common

template print(i) =
  echo indent("""
printf("""" & i & """");
#if defined(""" & i & """)
  printf("\t%d", """ & i & """);
#else
  printf("\tundefined");
#endif
printf("\n");
""", 2)

template print(i, m) =
  echo indent("""
printf("""" & i & """");
#if defined(""" & i & """)
  printf("\t%d", """ & i & """);
  """ & m & '\n' & """
#else
  printf("\tundefined\tundefined");
#endif
printf("\n");
""", 2)

proc printSysconfTest(m: string) =
  print m, """printf("\t%d", sysconf(""" & m & """));"""

proc printPathconfTest(m: string) =
  print m, """printf("\t%d", pathconf(""" & m & """, """" & currentSourcePath() & """"));"""

proc printConfstrTest(m: string) =
  print m, """n = confstr(""" & m & """, 0, 0) + 1;
  s = realloc(s, n);
  assert(confstr(""" & m & """, s, n) == n - 1);
  printf("\t%s", s);"""

proc main =
  var body = loadHeaderHtml("unistd.h")
  printGeneratedByComment("unistd.h", "//")
  echo """
#include <unistd.h>
#include <stdio.h>
#include <assert.h>

int main() {
  size_t n = 0;
  char** s = NULL;
"""
  print("_POSIX_SUBPROFILE")
  var i = 0
  for _ in body.next i:
    if body[i].tag == "h4":
      if body[i].innerText == "DESCRIPTION":
        var n = body.nextIdx(i+1).get
        let c = body[n]
        for e in c.betweenAll(("h5", some "@ 'Version Test Macros'"), ("dl", string.none), ("h5", some "@ 'Constants for Functions'")):
          for (k, _, _) in e.defs():
            print(k)
        splitLinesOfNextElement(("p", some "@ 'shall define the following symbolic constants for pathconf'"), printPathconfTest)
        splitLinesOfNextElement(("p", some "@ 'shall define the following symbolic constants for sysconf'"), printSysconfTest)
        n = c.nextMatchIdx(0, ("p", some "@ 'shall define the following symbolic constants for the confstr'")).get
        n = c.nextIdx(n+1).get
        for (dt, dd, _) in c[n].defs():
          printConfstrTest dt
        splitLinesOfNextElement(("p", some "@ 'following symbolic constants are reserved for compatibility'"), printConfstrTest)
  echo """
  if (s != NULL) {
    free(s);
  }
}"""
main()
