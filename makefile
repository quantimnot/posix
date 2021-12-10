default: posix/codes.nim posix/profiles.nim posix/subprofiles.nim posix/ffi/private/gen_typed_consts.nim

posix/profiles.nim: profiles
	mkdir -p posix
	./profiles > $@

posix/subprofiles.nim: subprofiles
	mkdir -p posix
	./subprofiles > $@

posix/codes.nim: codes
	mkdir -p posix
	./codes > $@

posix/ffi/private/gen_typed_consts.nim: scrape
	mkdir -p posix/ffi/private
	./scrape

scrape: build_tools/scrape.nim posix/codes.nim
	nim c -d:danger -o:$@ build_tools/scrape.nim

codes: build_tools/scrape_codes.nim
	nim c -d:danger -o:$@ build_tools/scrape_codes.nim

profiles: build_tools/scrape_profiles.nim
	nim c -d:danger -o:$@ build_tools/scrape_profiles.nim

subprofiles: build_tools/scrape_subprofiles.nim
	nim c -d:danger -o:$@ build_tools/scrape_subprofiles.nim

consts: scrape
	for c0 in out/*consts_0.nim; do \
		c1=$${c0%_*}_1.nim; \
		c=$${c0%_*}.nim; \
		nim r $$c0 > $$c1 && nim r $$c1 > $$c; \
	done

conformance.lst: conformance
	./conformance > $@

conformance: conformance.c
	$$CC -w conformance.c -o $@

conformance.c: build_tools/scrape_unistd.nim
	nim r $? > $@

posix_2004:
	mkdir -p $@
	cd $@ && { \
		curl -L https://pubs.opengroup.org/onlinepubs/009695399/download/susv3.tar.bz2 | tar xjf -; \
		mv susv3/* .; \
		rm -rf susv3; \
	}
posix_2008:
	mkdir -p $@
	cd $@ && { \
		curl -L https://pubs.opengroup.org/onlinepubs/9699919799.2008edition/download/susv4.tar.bz2 | tar xjf -; \
		mv susv4/* .; \
		rm -rf susv4; \
	}
posix_2013:
	mkdir -p $@
	cd $@ && { \
		curl -L https://pubs.opengroup.org/onlinepubs/9699919799.2013edition/download/susv4tc1.tar.bz2 | tar xjf -; \
		mv susv4tc1/* .; \
		rm -rf susv4tc1; \
	}
posix_2016:
	mkdir -p $@
	cd $@ && { \
		curl -L https://pubs.opengroup.org/onlinepubs/9699919799.2016edition/download/susv4tc2.tar.bz2 | tar xjf -; \
		mv susv4tc2/* .; \
		rm -rf susv4tc2; \
	}
posix_2018:
	mkdir -p $@
	cd $@ && { \
		curl -L https://pubs.opengroup.org/onlinepubs/9699919799/download/susv4-2018.zip | tar xf -; \
		mv susv4-2018/* .; \
		rm -rf susv4-2018; \
	}
all_posix_editions: posix_2004 posix_2008 posix_2013 posix_2016 posix_2018
