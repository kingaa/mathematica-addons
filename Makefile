INSTALLDIR = /usr/local/mathematica/AddOns/Applications

CC = gcc
CFLAGS = -O2 -I. -Wall -static
LDLIBS = -L. -L/usr/X11/lib
AR = ar
ARFLAGS = rs
INSTALL = install
PACKAGES = Banzhaf.m BifCurve.m Frechet.m Ideal.m NormalForm.m	\
Puiseux.m RatSimp.m Taylor.m Tr.m WriteBin.m

install: $(PACKAGES)
	$(INSTALL) -m0444 $(PACKAGES) $(INSTALLDIR)

init: init.m
	$(INSTALL) -m0400 init.m $(HOME)/.Mathematica/3.0/Kernel

%.tgz: %
	$(TAR) $(TARFLAGS) $*.tgz $*

%.gz: %
	$(GZIP) < $* > $*.gz

%: %.c $(INC) $(LIB)
	$(CC) $(CFLAGS) $*.c $(LDLIBS) -o $*

(%.o): %.c $(INC)
	$(CC) -c $(CFLAGS) $*.c -o $*.o
	$(AR) $(ARFLAGS) $@ $*.o
	$(RM) $*.o 

$(LIB):	$(LIB)($(addsuffix .o, $(basename $(SRC))))

clean:
	rcsclean

