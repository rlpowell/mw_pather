HASTEC_VERSION := $(shell hastec --version 2>/dev/null)

all:
ifdef HASTEC_VERSION
	hastec --make mw_pather.hs
	ghc --make mw_pather.hs
else
	chcon -t httpd_user_content_t -R /home/rlpowell/src/mw_pather/
	zip source.zip Makefile index.html mw_pather.hs
endif

clean:
	rm -r main mw_pather.js mw_pather.o mw_pather.hi
