
RACO=/Applications/Racket\ v6.3/bin/raco

all: test

test:
	$(RACO) test -x .

docs:
	$(RACO) scribble \
		--htmls \
		--dest docs \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org \
		scribblings/strappy.scrbl

link:
	$(RACO) pkg install --link -n strappy $$(pwd)

unlink:
	$(RACO) pkg remove strappy
