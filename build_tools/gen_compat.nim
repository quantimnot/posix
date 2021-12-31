import tables
import platforms
import ../posix/codes, ../posix/options

proc main =
  var file = open($os() & "_compat.nim", fmWrite)
  file.writeLine "# GENERATED for `" & $os() & "` platform by gen_compat.nim"
  file.writeLine "# "

main()
