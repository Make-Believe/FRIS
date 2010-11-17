.PHONY: all package install test

PACKAGE=ftdr
VER=0.1

PKG_NAME=$(PACKAGE)_$(VER)

all: package

package: 
	tar czf $(PKG_NAME).tar.gz $(PKG_NAME)

install: package
	sudo R CMD INSTALL $(PKG_NAME).tar.gz

test: install
	R
