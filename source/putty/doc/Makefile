all: man index.html

# Decide on the versionid policy.
#
# If the user has passed in $(VERSION) on the command line (`make
# VERSION="Release 0.56"'), we use that as an explicit version string.
# Otherwise, we use `svnversion' to examine the checked-out
# documentation source, and if that returns a single revision number
# then we invent a version string reflecting just that number. Failing
# _that_, we resort to versionids.but which gives 'version
# unavailable'.
#
# So here, we define VERSION using svnversion if it isn't already
# defined ...
ifndef VERSION
SVNVERSION=$(shell test -d .svn && svnversion .)
BADCHARS=$(findstring :,$(SVNVERSION))$(findstring S,$(SVNVERSION))
ifeq ($(BADCHARS),)
ifneq ($(SVNVERSION),)
ifneq ($(SVNVERSION),exported)
VERSION=Built from revision $(patsubst M,,$(SVNVERSION))
endif
endif
endif
endif
# ... and now, we condition our build behaviour on whether or not
# VERSION _is_ defined.
ifdef VERSION
VERSIONIDS=vstr
vstr.but: FORCE
	printf '\\versionid $(VERSION)\n' > vstr.but
FORCE:;
else
VERSIONIDS=vids
endif

CHAPTERS := $(SITE) copy blurb intro gs using config pscp psftp plink
CHAPTERS += pubkey pageant errors faq feedback pubkeyfmt licence udp
CHAPTERS += pgpkeys sshnames
CHAPTERS += index $(VERSIONIDS)

INPUTS = $(patsubst %,%.but,$(CHAPTERS))

# This is temporary. Hack it locally or something.
HALIBUT = halibut

index.html: $(INPUTS)
	$(HALIBUT) --text --html --chm $(INPUTS)

# During formal builds it's useful to be able to build this one alone.
putty.chm: $(INPUTS)
	$(HALIBUT) --chm $(INPUTS)

# We don't ship this any more.
putty.hlp: $(INPUTS)
	$(HALIBUT) --winhelp $(INPUTS)

putty.info: $(INPUTS)
	$(HALIBUT) --info $(INPUTS)

MKMAN = $(HALIBUT) --man=$@ mancfg.but $<
MANPAGES = putty.1 puttygen.1 plink.1 pscp.1 psftp.1 puttytel.1 pterm.1 \
           pageant.1 psocks.1 psusan.1
man: $(MANPAGES)

putty.1: man-putty.but mancfg.but; $(MKMAN)
puttygen.1: man-puttygen.but mancfg.but; $(MKMAN)
plink.1: man-plink.but mancfg.but; $(MKMAN)
pscp.1: man-pscp.but mancfg.but; $(MKMAN)
psftp.1: man-psftp.but mancfg.but; $(MKMAN)
puttytel.1: man-puttytel.but mancfg.but; $(MKMAN)
pterm.1: man-pterm.but mancfg.but; $(MKMAN)
pageant.1: man-pageant.but mancfg.but; $(MKMAN)
psocks.1: man-psocks.but mancfg.but; $(MKMAN)
psusan.1: man-psusan.but mancfg.but; $(MKMAN)

mostlyclean:
	rm -f *.html *.txt *.hlp *.cnt *.1 *.info vstr.but *.hh[pck]
clean: mostlyclean
	rm -f *.chm
