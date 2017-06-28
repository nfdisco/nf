guileversion = 2.0

prefix = $(HOME)/.local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
datarootdir = $(prefix)/share

sitedir = $(datarootdir)/guile/site/$(guileversion)
ccachedir = $(libdir)/guile/site/$(guileversion)/site-ccache

define deletedirs
test -d $(sitedir) && cd $(datarootdir) && \
  rmdir -p guile/site/$(guileversion) || true; \
test -d $(ccachedir) && cd $(libdir) && \
  rmdir -p guile/site/$(guileversion)/site-ccache || true
endef

define reminder
echo "" ; \
echo "*** Guile source files installed at:" ;\
echo "*** $(sitedir)." ; \
echo "*** Guile compiled files installed at:" ;\
echo "*** $(ccachedir)." ; \
echo "***" ; \
echo "*** You may need to change the value of the environment variables" ; \
echo "*** GUILE_LOAD_PATH and GUILE_LOAD_COMPILED_PATH accordingly." ; \
echo ""
endef

objects = src/slug.go src/nf.go

install-slug: src/slug.go
	install -m 644 -D -t $(sitedir)/slug src/slug.scm
	install -m 644 -D -t $(ccachedir)/slug src/slug.go
	@$(reminder)

uninstall-slug:
	-rm -f $(sitedir)/slug/slug.scm
	-rm -f $(ccachedir)/slug/slug.go
	-test -d $(sitedir)/slug && rmdir $(sitedir)/slug || true
	-test -d $(ccachedir)/slug && rmdir $(ccachedir)/slug || true
	$(deletedirs)

install: install-slug src/nf.go
	install -m 755 -D -t $(bindir) src/nf
	install -m 644 -D -t $(ccachedir)/$(bindir) src/nf.go
	@$(reminder)

uninstall: uninstall-slug
	-rm -f $(bindir)/nf
	-rm -f $(ccachedir)/$(bindir)/nf.go
	-cd $(ccachedir) && rmdir -p ./$(bindir) || true
	$(deletedirs)

clean:
	-rm -f $(objects)

src/slug.go:
	guild compile -o src/slug.go src/slug.scm

src/nf.go: install-slug
	GUILE_LOAD_PATH=$(sitedir) GUILE_LOAD_COMPILED_PATH=$(ccachedir) \
	  guild compile -o src/nf.go src/nf

all: $(objects)

.PHONY: all clean install uninstall install-slug uninstall-slug
