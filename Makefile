NAME  := weechat
DESCRIPTION := Chat via WeeChat\'s relay protocol in Emacs

EMACS := emacs
BATCH := $(EMACS) --batch --eval '(progn (require (quote package)) (package-initialize) (add-to-list (quote load-path) "."))'

DATE := $(shell date +%Y%m%d)

ifneq ($(wildcard .git),)
	GITVERSION ?= $(shell git describe --tags --dirty --always)
	VERSION ?= $(shell git describe --tags --exact-match 2> /dev/null | sed 's/^v//')
endif

ifeq ($(VERSION),)
	ifneq ($(wildcard version),)
		VERSION := $(shell cat version)
	else
		VERSION := $(DATE)
	endif
endif

PACKAGE := $(NAME)-$(VERSION)
TARBALL := $(PACKAGE).tar
PKG_EL := $(NAME)-pkg.el
PKG_EL_IN := $(PKG_EL).in

TESTS   := $(wildcard *-test.el)
EL 	:= $(wildcard weechat*.el)
SOURCES := $(filter-out $(PKG_EL),$(filter-out $(TESTS),$(EL)))
PACKAGE_CONTENT := $(SOURCES) Makefile README.org README.html

.PHONY: all test doc package clean distclean
all: package
	$(info $(NAME) Version: $(VERSION))

test: $(TESTS)
	@$(BATCH) -l ert.el $(foreach file,$^,-l $(file)) -f ert-run-tests-batch-and-exit

clean:
	$(info Cleaning up)
	@$(RM) $(NAME)-pkg.el README.html README.txt
	@$(RM) -r $(PACKAGE)
	@$(RM) $(TARBALL)

distclean: clean
	@$(RM) -r $(TARBALL)

README.html: README.org
	$(info Creating documentation: $@)
	@$(BATCH) -l org.el --visit=$< -f org-html-export-to-html

doc: README.html

$(PKG_EL): $(PKG_EL_IN)
	sed -e s/@VERSION@/$(VERSION)/ $< > $@

$(TARBALL): $(PKG_EL) doc
	$(info Creating package tarball $(TARBALL))
	@mkdir -p $(PACKAGE)
	@cp -r $(PACKAGE_CONTENT) $(PKG_EL) $(PACKAGE)/
	@echo "$(VERSION)" > $(PACKAGE)/version
ifneq ($(GITVERSION),)
	@echo "$(GITVERSION)" > $(PACKAGE)/git-version
endif
	@tar cf $(TARBALL) $(PACKAGE)

package: $(TARBALL)
