# where the url lisp files should go
srcdir  = .
prefix  = /opt/home/papo
datadir = ${prefix}/share
lispdir = $(prefix)/share/xemacs/site-lisp
infodir = ${prefix}/share/info

SUBDIRS =lisp texi

INSTALL   = /usr/bin/install -c
RM        = rm -f

.PHONY: $(SUBDIRS)

all:	url info

url:
	cd lisp && $(MAKE) $@

fast: info
	cd lisp && $(MAKE) $@

html info dvi:
	cd texi && $(MAKE) $@

install: all
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done

distclean: clean
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done
	$(RM) config.* Makefile

clean:
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done

Makefile: $(srcdir)/Makefile.in ./config.status
	( cd . ; CONFIG_FILES=Makefile ./config.status )

$(srcdir)/configure: $(srcdir)/configure.in $(srcdir)/aclocal.m4
	cd $(srcdir) && autoreconf

./config.status: $(srcdir)/configure
	cd . && ./config.status --recheck

###############################################################################
# Distribution building, the joy of my life
###############################################################################
SUPPORTFILES =  HOWTO TODO README INSTALL README.NT README.VMS BUGS
DISTFILES    =  configure.in aclocal.m4 Makefile.in install-sh	\
		configure descrip.mms ChangeLog	build.bat 	\
		$(SUPPORTFILES)
MAJOR = 2
MINOR = 0
VERSION = $(MAJOR).$(MINOR).$(shell cat .version)
DIRNAME = url-$(VERSION)

newversion:
	if [ ! -f .version ]; then \
		echo 1 > .version; \
	else \
		expr 0`cat .version` + 1 > .version; \
	fi
	#cvs commit -m "" .version

version: newversion clean $(DISTFILES)
	echo "Building version $(VERSION)"
	./build-dist --version=$(VERSION)

realversion: $(DISTFILES)
	mkdir /tmp/$(DIRNAME)
	cp $(DISTFILES) /tmp/$(DIRNAME)
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) version VERSION=$(VERSION) DIRNAME=/tmp/$(DIRNAME) ); done
	tar cf - contrib | ( cd /tmp/$(DIRNAME) ; tar xf - )
	( cd /tmp ; tar cf - $(DIRNAME) | gzip -c -9 > /tmp/$(DIRNAME).tar.gz )
	rm -fr /tmp/$(DIRNAME)
