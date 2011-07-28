LISPSCRIPT=sbcl --script

.PHONY: test

test.crf: test.mdl
	./wapiti-munge.pl $< > $@

test:
	prove -e '$(LISPSCRIPT)' -r t/
