
all: test

test:
	raco test -x .

docs: scribblings/*.scrbl
	raco scribble \
		--html \
		--dest docs \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org \
		scribblings/strappy.scrbl

link:
	raco pkg install --link -n strappy $$(pwd)

unlink:
	raco pkg remove strappy
