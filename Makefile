INSTALLDIR = $(HOME)/.Mathematica/Applications
PUBLISHDIR = $(HOME)/www-home/pub/math/

CC = gcc
CFLAGS = -O2 -I. -Wall -static
LDLIBS = -L. -L/usr/X11/lib
AR = ar
ARFLAGS = rs
TAR = tar
TARFLAGS = zcvf
CP = cp
INSTALL = install
PACKAGES = BifCurve.m Frechet.m Ideal.m NormalForm.m Puiseux.m	\
Taylor.m WriteBin.m

unpack: $(PACKAGES) init.m README

publish: $(PACKAGES) init.m README
	$(INSTALL) -m0644 $^ $(PUBLISHDIR)
	cd $(PUBLISHDIR); sh publish.sh

install: $(PACKAGES)
	 $(INSTALL) -m0444 $(PACKAGES) $(INSTALLDIR)

init: init.m
	$(INSTALL) -d -m0700 $(HOME)/.Mathematica/Kernel
	$(INSTALL) -m0400 init.m $(HOME)/.Mathematica/Kernel

dist: unpack VERSION Makefile README math.el
	mkdir addons-$(shell cat VERSION)
	$(CP) Makefile $(PACKAGES) init.m README math.el addons-$(shell cat VERSION)
	$(TAR) $(TARFLAGS) addons-$(shell cat VERSION).tar.gz addons-$(shell cat VERSION)
	$(RM) -r addons-$(shell cat VERSION) VERSION

VERSION:
	$(CO) -kv $@

%.tar.gz: %
	$(TAR) $(TARFLAGS) $*.tar.gz $*

%.gz: %
	$(GZIP) < $* > $*.gz
