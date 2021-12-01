## PURPOSE
##   Common code used by all posix versions.
import os, scrape_common
export os, scrape_common

template basenames*(glob: string): string =
  walkFiles(glob)

iterator functions*(glob: string): string =
  for f in basenames(glob):
    yield f.splitFile.name

iterator basedefs*(glob: string): tuple[name: string, body: var XmlNode] =
  for f in basenames(glob):
    yield (f.splitFile.name, loadHtml(f).child("html").child("body"))
