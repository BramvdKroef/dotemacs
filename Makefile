EMACS=emacs
SITEFLAG=--no-site-file
SOURCE=$(wildcard *.el)
TARGET=$(patsubst %.el,%.elc,$(SOURCE))

.PHONY: all clean
.PRECIOUS: %.elc
all: $(TARGET)

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch \
		-l loadpaths.el \
		-f batch-byte-compile $<

clean:
	-rm -f *~ *.elc emms-auto.el
