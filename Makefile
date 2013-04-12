PKG=ftdr
VER=0.1
PKG_VER=$(PKG)_$(VER)
PKG_TGZ="../$(PKG_VER).tar.gz"

.PHONY:	package install

$(PKG_TGZ):$(PKG_VER)/src/$(PKG).c
	tar -czvf $(PKG_TGZ) $(PKG_VER)

package: $(PKG_TGZ)

install: package
	sudo R CMD INSTALL $(PKG_TGZ)

debug: install
	R -d gdb 
