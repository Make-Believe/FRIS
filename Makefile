.PHONY: all package install test

PACKAGE=ftdr
VER=0.1

PKG_NAME=$(PACKAGE)_$(VER)
CFLAGS=-g -O0

all: package

package: 
	tar czf $(PKG_NAME).tar.gz $(PKG_NAME)

install: package
	CFLAGS="$(CFLAGS)" sudo R CMD INSTALL $(PKG_NAME).tar.gz

test: install
	R
