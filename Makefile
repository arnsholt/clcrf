.PHONY: test

test.crf: test.mdl
	./wapiti-munge.pl $< > $@

test:
	prove -e '' -r t/
