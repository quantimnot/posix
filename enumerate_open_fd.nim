import "./posix"

var rlim: RLimit
var stat: Stat

if posix.getrlimit(posix.RLIMIT_NOFILE, rlim) != posix.ESUCCESS:
  quit "Unexpected error trying to get open file limit.", 1

echo "fd\t  \tinode\n==\t  \t====="
for fd in 0..rlim.rlim_cur:
  if fstat(fd.cint, stat) != ESUCCESS:
    if errno != EBADF:
      quit "Unexpected error trying to get information about file descriptor: " & $fd
  else:
    echo $fd & "\t->\t" & $stat.st_ino
