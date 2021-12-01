default: scrape.nim ctypeof.nim
	nim r scrape
	for c0 in out/*consts_0.nim; do \
		c1=$${c0%_*}_1.nim; \
		c=$${c0%_*}.nim; \
		nim r $$c0 > $$c1 && nim r $$c1 > $$c; \
	done

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
