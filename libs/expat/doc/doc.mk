# Copyright (C) Sebastian Pipping <sebastian@pipping.org>
# Licensed under MIT/X License

DOCBOOK_TO_MAN = docbook2x-man

srcdir = .


.PHONY: all
all: xmlwf.1

xmlwf.1: XMLWF.1
	cp $< $@

XMLWF.1: $(srcdir)/xmlwf.xml
	$(DOCBOOK_TO_MAN) $(srcdir)/xmlwf.xml

.PHONY: clean
clean:
	$(RM) xmlwf.1 XMLWF.1
